alias noreturn = typeof(*null);

extern (C) noreturn exit();

/*****************************************/

bool testf(int i)
{
    return i && assert(0);
}

bool testt(int i)
{
    return i || assert(0);
}

int testa(int i)
{
    if (i && exit())
        return i + 1;
    return i - 1;
}

int testb(int i)
{
    if (i || exit())
        return i + 1;
    return i - 1;
}

void test1()
{
    assert(testf(0) == false);
    assert(testt(1) == true);

    assert(testa(0) == -1);
    assert(testb(3) == 4);
}

/*****************************************/

noreturn exit1() { assert(0); }
noreturn exit2() { assert(0); }


int heli1(int i)
{
    return i ? exit1() : i - 1;
}

int heli2(int i)
{
    return i ? i - 1 : exit1();
}

noreturn heli3(int i)
{
    return i ? exit1() : exit2();
}

void test2()
{
    assert(heli1(0) == -1);
    assert(heli2(1) == 0);
}

/*****************************************/

struct BasicStruct
{
	int firstInt;
	noreturn noRet;
	long lastLong;
}

struct AlignedStruct
{
	int firstInt;
	align(16) noreturn noRet;
	long lastLong;
}

void takeBasic(BasicStruct bs)
{
    assert(bs.firstInt == 13);
    assert(bs.lastLong == 42);

    assert(&bs.noRet == (&bs.firstInt + 1));
}

void takeAligned(AlignedStruct as)
{
    assert(as.firstInt == 99);
    assert(as.lastLong == 0xDEADBEEF);

    assert(&as.noRet == &as.lastLong);
}

void test3()
{
    {
        BasicStruct bs;
        bs.firstInt = 13;
        bs.lastLong = 42;
        takeBasic(bs);
    }
    {
        AlignedStruct as;
        as.firstInt = 99;
        as.lastLong = 0xDEADBEEF;
        takeAligned(as);
    }
}

/*****************************************/

Exception collectException(void function() f)
{
    try
    {
        f();
        return null;
    }
    catch (Exception e)
        return e;
}


int return_()
{
    return throw new Exception("Return");
}

void ternary(int i)
{
    i > 0 ? i++ : throw new Exception("Ternary");
}

void call()
{
    ternary(throw new Exception("Call"));
}

void arrayLiteral()
{
    int[] arr = [
        1,
        throw new Exception("ArrayLiteral"),
        2
    ];
}

void assocArrayLiteral()
{
    int[string] arr = [
        "A": 1,
        "B": throw new Exception("AssocArrayLiteral"),
        "C": 2
    ];
}

void testThrowExpression()
{
    Exception ae = collectException({ return_(); });
    assert(ae);

    ae = collectException({ ternary(1); });
    assert(!ae);

    ae = collectException({ ternary(-1); });
    assert(ae);

    ae = collectException(&call);
    assert(ae);
    assert(ae.msg == "Call");

    ae = collectException(&arrayLiteral);
    assert(ae);

    ae = collectException(&assocArrayLiteral);
    assert(ae);
}


/*****************************************/

/// Verify that throws does not screw with side effects
void testThrowSideEffect()
{
    static void foo(bool, void*, int) {}

    bool b;
    int i;

    try
    {
        foo(b = true, throw new Exception(""), i++);
        assert(false);
    }
    catch (Exception) {}

    assert(b == true);
    assert(i == 0);
}

/// Verify that throws does not screw with dtors
void testThrowDtor()
{
    static struct S
    {
        __gshared int destructed;
        int id;

        ~this()
        {
            assert(!(destructed & id));
            destructed |= id;
        }

        string getMessage()
        {
            // Force runtime dependency
            return destructed ? "Already destructed" : "Valid";
        }
    }

    static void fooD(S, int, S) {}
    bool caught;

    try
    {
        fooD(S(1), throw new Exception(S(2).getMessage()), S(4));
        assert(false);
    }
    catch (Exception e)
    {
        caught = true;
        assert(e.msg == "Valid");
    }
    assert(caught);
    assert(S.destructed == (1 | 2));


    static void fooC(S, int, S) {}
    caught = false;
    S.destructed = 0;

    try
    {
        fooC(S(1), throw new Exception(S(2).getMessage()), S(4));
        assert(false);
    }
    catch (Exception e)
    {
        caught = true;
        assert(e.msg == "Valid");
    }
    assert(caught);
    assert(S.destructed == (1 | 2));
}

/*****************************************/

noreturn func()
{
    throw new Exception("B");
}

// https://issues.dlang.org/show_bug.cgi?id=23120
void test23120()
{
    string a;
    try
    {
        noreturn q = throw new Exception ("A");
    }
    catch(Exception e)
    {
        a ~= e.msg;
    }

    try
    {
        noreturn z = func();
    }
    catch(Exception e)
    {
        a ~= e.msg;
    }

    assert(a == "AB");
}

/*****************************************/
int main()
{
    test1();
    test2();
    test3();
    testThrowExpression();
    testThrowSideEffect();
    testThrowDtor();
    test23120();
    return 0;
}
