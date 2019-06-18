// PERMUTE_ARGS: -O -fPIC

extern(C) int printf(const char*, ...);

/****************************************************/

class Abc : Exception
{
    this()
    {
        super("");
    }
    int i;
}

int y;

alias int boo;

void foo(int x)
{
    y = cast(boo)1;
L6:
    try
    {
        printf("try 1\n");
        y += 4;
        if (y == 5)
            goto L6;
        y += 3;
    }
    finally
    {
        y += 5;
        printf("finally 1\n");
    }
    try
    {
        printf("try 2\n");
        y = 1;
        if (y == 4)
            goto L6;
        y++;
    }
    catch (Abc c)
    {
        printf("catch 2\n");
        y = 2 + c.i;
    }
    y++;
    printf("done\n");
}

/****************************************************/


class IntException : Exception
{
    this(int i)
    {
        m_i = i;
    super("");
    }

    int getValue()
    {
        return m_i;
    }

    int m_i;
}


void test2()
{
    int     cIterations =   10;

    int     i;
    long    total_x     =   0;
    long    total_nox   =   0;

    for(int WARMUPS = 2; WARMUPS-- > 0; )
    {
        for(total_x = 0, i = 0; i < cIterations; ++i)
        {
            total_nox += fn2_nox();
        }
printf("foo\n");

        for(total_nox = 0, i = 0; i < cIterations; ++i)
        {
printf("i = %d\n", i);
            try
            {
                int z = 1;

                throw new IntException(z);
            }
            catch(IntException x)
            {
printf("catch, i = %d\n", i);
                total_x += x.getValue();
            }
        }
    }

    printf("iterations %d totals: %lld, %lld\n", cIterations, total_x, total_nox);
}

int fn2_nox()
{
    return 47;
}


/****************************************************/

void test3()
{
    static int x;
    try
    {
    }
    finally
    {
        printf("a\n");
        assert(x == 0);
        x++;
    }
    printf("--\n");
    assert(x == 1);
    try
    {
        printf("tb\n");
        assert(x == 1);
    }
    finally
    {
        printf("b\n");
        assert(x == 1);
        x++;
    }
    assert(x == 2);
}

/****************************************************/

class Tester
{
    this(void delegate() dg_) { dg = dg_; }
    void delegate() dg;
    void stuff() { dg(); }
}

void test4()
{
    printf("Starting test\n");

    int a = 0;
    int b = 0;
    int c = 0;
    int d = 0;

    try
    {
        a++;
        throw new Exception("test1");
        a++;
    }
    catch(Exception e)
    {
        auto es = e.toString();
                printf("%.*s\n", cast(int)es.length, es.ptr);
        b++;
    }
    finally
    {
        c++;
    }

    printf("initial test.\n");

    assert(a == 1);
    assert(b == 1);
    assert(c == 1);

    printf("pass\n");

    Tester t = new Tester(
    delegate void()
    {
        try
        {
            a++;
            throw new Exception("test2");
            a++;
        }
        catch(Exception e)
        {
            b++;
            throw e;
            b++;
        }
    });

    try
    {
        c++;
        t.stuff();
        c++;
    }
    catch(Exception e)
    {
        d++;
        string es = e.toString;
        printf("%.*s\n", cast(int)es.length, es.ptr);
    }

    assert(a == 2);
    assert(b == 2);
    assert(c == 2);
    assert(d == 1);


    int q0 = 0;
    int q1 = 0;
    int q2 = 0;
    int q3 = 0;

    Tester t2 = new Tester(
    delegate void()
    {
        try
        {
            q0++;
            throw new Exception("test3");
            q0++;
        }
        catch(Exception e)
        {
            printf("Never called.\n");
            q1++;
            throw e;
            q1++;
        }
    });

    try
    {
        q2++;
        t2.stuff();
        q2++;
    }
    catch(Exception e)
    {
        q3++;
                string es = e.toString;
        printf("%.*s\n", cast(int)es.length, es.ptr);
    }

    assert(q0 == 1);
    assert(q1 == 1);
    assert(q2 == 1);
    assert(q3 == 1);

    printf("Passed!\n");
}

/****************************************************/

void test5()
{
    char[] result;
    int i = 3;
    while(i--)
    {
        try
        {
            printf("i: %d\n", i);
            result ~= 't';
            if (i == 1)
                continue;
        }
        finally
        {
            printf("finally\n");
            result ~= cast(char)('a' + i);
        }
    }
    printf("--- %.*s", cast(int)result.length, result.ptr);
    if (result != "tctbta")
        assert(0);
}

/****************************************************/

void test6()
{
    char[] result;

    while (true)
    {
        try
        {
            printf("one\n");
            result ~= 'a';
            break;
        }
        finally
        {
            printf("two\n");
            result ~= 'b';
        }
    }
    printf("three\n");
    result ~= 'c';
    if (result != "abc")
        assert(0);
}

/****************************************************/

string a7;

void doScan(int i)
{
    a7 ~= "a";
    try
    {
        try
        {
            a7 ~= "b";
            return;
        }
        finally
        {
            a7 ~= "c";
        }
    }
    finally
    {
        a7 ~= "d";
    }
}

void test7()
{
    doScan(0);
    assert(a7 == "abcd");
}


/****************************************************
 * Exception chaining tests. See also test4.d
 * Don writes about the complexity:

I can explain this, since I did the original implementation.
When I originally implemented this, I discovered that the idea of
"chained exceptions" was hopeless naive. The idea was that while
processing one exception, if you encounter a second one, and you
chain them together. Then you get a third, fourth, etc.

The problem is that it's much more complicated than that. Each of
the exceptions can be a chain of exceptions themselves. This means
that you don't end up with a chain of exceptions, but rather a tree
of exceptions. That's why there are those really nasty test cases
in the test suite.

The examples in the test suite are very difficult to understand if
you expect it to be a simple chain!

On the one hand, I was very proud that I was able to work out the
barely-documented behaviour of Windows SEH, and it was really
thorough. In the initial implementation, all the complexity
was covered. It wasn't the bugfix-driven-development which dmd
usually operates under <g>.

But on the other hand, once you can see all of the complexity,
exception chaining becomes much less convincing as a concept. Sure,
the full exception tree is available in the final exception which
you catch. But, is it of any use? I doubt it very much.
It's pretty clearly a nett loss to the language, it increases
complexity with negligible benefit. Fortunately in this case, the
cost isn't really high.

https://digitalmars.com/d/archives/digitalmars/D/Dicebot_on_leaving_D_It_is_anarchy_driven_development_in_all_its_317950.html#N318305
 ****************************************************/
int result1513;

void bug1513a()
{
     throw new Exception("d");
}

void bug1513b()
{
    try
    {
        try
        {
            bug1513a();
        }
        finally
        {
            result1513 |=4;
           throw new Exception("f");
        }
    }
    catch(Exception e)
    {
        assert(e.msg == "d");
        assert(e.next.msg == "f");
        assert(!e.next.next);
        int i;
        foreach (u; e)
        {
            if (i)
                assert(u.msg == "f");
            else
                assert(u.msg == "d");
            ++i;
        }
    }
}

void bug1513c()
{
    try
    {
        try
        {
            throw new Exception("a");
        }
        finally
        {
            result1513 |= 1;
            throw new Exception("b");
        }
    }
    finally
    {
        bug1513b();
        result1513 |= 2;
        throw new Exception("c");
    }
}

void bug1513()
{
    result1513 = 0;
    try
    {
        bug1513c();
    }
    catch(Exception e)
    {
        assert(result1513 == 7);
        assert(e.msg == "a");
        assert(e.next.msg == "b");
        assert(e.next.next.msg == "c");
    }
}

void collideone()
{
    try
    {
        throw new Exception("x");
    }
    finally
    {
        throw new Exception("y");
    }
}

void doublecollide()
{
    try
    {
        try
        {
            try
            {
                throw new Exception("p");
            }
            finally
            {
                throw new Exception("q");
            }
        }
        finally
        {
            collideone();
        }
    }
    catch(Exception e)
    {
            assert(e.msg == "p");
            assert(e.next.msg == "q");
            assert(e.next.next.msg == "x");
            assert(e.next.next.next.msg == "y");
            assert(!e.next.next.next.next);
    }
}

void collidetwo()
{
       try
        {
            try
            {
                throw new Exception("p2");
            }
            finally
            {
                throw new Exception("q2");
            }
        }
        finally
        {
            collideone();
        }
}

void collideMixed()
{
    int works = 6;
    try
    {
        try
        {
            try
            {
                throw new Exception("e");
            }
            finally
            {
                throw new Error("t");
            }
        }
        catch(Exception f)
        {    // Doesn't catch, because Error is chained to it.
            works += 2;
        }
    }
    catch(Error z)
    {
        works += 4;
        assert(z.msg=="t"); // Error comes first
        assert(z.next is null);
        assert(z.bypassedException.msg == "e");
    }
    assert(works == 10);
}

class AnotherException : Exception
{
    this(string s)
    {
        super(s);
    }
}

void multicollide()
{
    try
    {
       try
        {
            try
            {
                try
                {
                    throw new Exception("m2");
                }
                finally
                {
                    throw new AnotherException("n2");
                }
            }
            catch(AnotherException s)
            {   // Not caught -- we needed to catch the root cause "m2", not
                // just the collateral "n2" (which would leave m2 uncaught).
                assert(0);
            }
        }
        finally
        {
            collidetwo();
        }
    }
    catch(Exception f)
    {
        assert(f.msg == "m2");
        assert(f.next.msg == "n2");
        Throwable e = f.next.next;
        assert(e.msg == "p2");
        assert(e.next.msg == "q2");
        assert(e.next.next.msg == "x");
        assert(e.next.next.next.msg == "y");
        assert(!e.next.next.next.next);
    }
}

/****************************************************/

void use9568(char [] x, char [] y) {}

int bug9568()
{
    try
        return 7;
     finally
        use9568(null,null);
}

void test9568()
{
    assert( bug9568() == 7 );
}

/****************************************************/

version (DigitalMars)
{
void test8a()
{
  int a;
  goto L2;    // L2 is not addressable.

  try {
      a += 2;
  }
  catch (Exception) {
      a += 3;
L2: ;
      a += 100;
  }
  assert(a == 100);
}

void test8b()
{
  int a;
  goto L2;    // L2 is not addressable.

  try {
  }
  catch (Exception) {
      a += 3;
L2: ;
      a += 100;
  }
  assert(a == 100);
}

void test8c()
{
  int a;
  goto L2;    // L2 is not addressable.

  try
    static assert(true);
  catch (Exception) {
      a += 3;
L2: ;
      a += 100;
  }
  assert(a == 100);
}

void test8()
{
  test8a();
  test8b();
  test8c();
}
}

/****************************************************/

uint foo9(uint i)
{
    try
    {
        ++i;
        return 3;
    }
    catch (Exception e)
    {
        debug printf("Exception happened\n");
    }
    return 4;
}

void test9()
{
    assert(foo9(7) == 3);
}

/****************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10964

void test10964()
{
    static struct S
    {
        this(this)
        {
            throw new Exception("BOOM!");
        }
    }

    S    ss;
    S[1] sa;
    int result;

    result = 0;
    try
    {
        ss = ss;
    }
    catch (Exception e) result = 1;
    catch (Error     e) result = 2;
    catch (Throwable e) result = 3;
    assert(result == 1);

    try
    {
        sa = ss;
    }
    catch (Exception e) result = 1;
    catch (Error     e) result = 2;
    catch (Throwable e) result = 3;
    assert(result == 1);

    try
    {
        sa = sa;
    }
    catch (Exception e) result = 1;
    catch (Error     e) result = 2;
    catch (Throwable e) result = 3;
    assert(result == 1);
}

/****************************************************/

alias Action = void delegate();

class A10
{
    invariant()
    {
    }

    public Action foo(Action a)
    {
        synchronized
        {
            B10 elements = new B10;
            Action[] actions = [a];

            elements.bar(actions);

            if (actions.length > 1)
                elements.bar(actions);
            return actions[0];
        }
        return null;
    }
}

class B10
{
    public bool bar(ref Action[])
    {
        return false;
    }
}

class D10
{
    void baz()
    {
    }
}

void test12989()
{
    auto a = new A10;
    auto d = new D10;

    assert(a.foo(&d.baz) == &d.baz);
}

/****************************************************/

int bar10(int c)
{
    if (c <= 0xFFFF)
    {
    L3:
        return 3;
    }
    throw new Exception("msg");
    goto L3;
}

void test10()
{
    int x;
    try
    {
        bar10(0x110000);
    }
    catch (Exception e)
    {
        printf("caught\n");
        x = 1;
    }
    assert(x == 1);
    printf("test10 success\n");
}

/****************************************************/

class ParseException : Exception
{
    @safe pure nothrow this( string msg )
    {
        super( msg );
    }
}

class OverflowException : Exception
{
    @safe pure nothrow this( string msg )
    {
        super( msg );
    }
}

void test11()
{
    int x;
    try
    {
        printf("test11()\n");
        throw new ParseException("msg");
    }
    catch( OverflowException e )
    {
        printf( "catch OverflowException\n" );
    }
    catch( ParseException e )
    {
        printf( "catch ParseException: %.*s\n", cast(int) e.msg.length, e.msg.ptr );
        x = 1;
    }
    assert(x == 1);
}

/****************************************************/
// https://issues.dlang.org/show_bug.cgi?id=17481

class C17481
{
    synchronized void trigger(){ new ubyte[1]; }
}

void test17481()
{
    auto k = new shared C17481;
    k.trigger;
}

/****************************************************/

// a nothrow function, even though it is not marked as nothrow
void test12()
{
    int i = 3;
    try
    {
        try
        {
            ++i;
            goto L10;
        }
        finally
        {
            i *= 2;
            printf("f1\n");
        }
    }
    finally
    {
        i += 5;
        printf("f2\n");
    }

L10:
    printf("3\n");
    assert(i == (3 + 1) * 2 + 5);
}

/****************************************************/

void foo13() { }

void test13()
{
    int i = 3;
    try
    {
        try
        {
            foo13(); // compiler assumes it throws
            ++i;
            goto L10;
        }
        finally
        {
            i *= 2;
            printf("f1\n");
        }
    }
    finally
    {
        i += 5;
        printf("f2\n");
    }

L10:
    printf("3\n");
    assert(i == (3 + 1) * 2 + 5);
}

/****************************************************/

// https://issues.dlang.org/show_bug.cgi?id=10966

void bug10966a(void* p)
{
    void* pstart = p;

    try
    {
        p = null;
        throw new Exception("dummy");
    }
    catch (Throwable o)
    {
        assert(p != pstart);
    }
}

void bug10966b()
{
    int x = 0;
    int i = 0;
    try
    {
        i = 1;
        throw new Exception("dummy");
    }
    catch (Throwable o)
    {
        x = i;
    }
    assert(x == 1);
}

void test10966()
{
    int s;
    bug10966a(&s);
    bug10966b();
}

/****************************************************/

// https://issues.dlang.org/show_bug.cgi?id=11049

void test11049()
{
    int[] arr = [1,2,3];

#line 4100 "foo"
    try { auto n = arr[3]; }
    catch (Error e)
    {
        //printf("e.file = %s\n", e.file.ptr);
        assert(e.file == "foo");  // fails
        assert(e.line == 4100);
    }

#line 4200 "bar"
    try { auto a = arr[3..9]; }
    catch (Error e)
    {
        //printf("e.file = %s\n", e.file.ptr);
        assert(e.file == "bar");  // fails
        assert(e.line == 4200);
    }
}

/****************************************************/

int main()
{
    printf("start\n");
    foo(3);
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();

    bug1513();
    doublecollide();
    collideMixed();
    multicollide();
    test9568();

    version(DigitalMars) test8();
    test9();
    test10964();
    test12989();
    test10();
    test11();
    test17481();
    test12();
    test13();
    test10966();
    test11049();

    printf("finish\n");
    return 0;
}
