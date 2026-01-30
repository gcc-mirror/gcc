mixin template OD(string s)
{

    string opDispatch(string name)() if (name == s)
    {
        return name;
    }
}

mixin template ODA(string s)
{
    void opDispatch(string name)(int x) if (name == s)
    {
        this.val = x;
    }
}

struct T
{
    mixin OD!"x";
    mixin OD!"y";
}

struct TAssign
{
    int val;
    mixin ODA!"x";
    mixin ODA!"y";
}

struct U
{
    mixin OD!"z";
}


template adder()
{
    int opBinary(string s : "+")(int x) { return x; }
}

template subtracter()
{
    int opBinary(string s : "-")(int x) { return -x; }
}


struct Arithmetic
{
    mixin adder;
    mixin subtracter;

}

template adderRight()
{
    int opBinaryRight(string s : "+")(int x){ return x; }
}


template subtracterRight()
{
    int opBinaryRight(string s: "-")(int x){ return -x; }
}

struct ArithmeticRight
{
    mixin adderRight;
    mixin subtracterRight;
}

template mixinOpAssign(string op)
{
    void opOpAssign(string s)(int x) if(s == op)
    {
        val = x;
        lastOp = s;
    }
}

struct AssignOverloads
{
    int val;
    string lastOp;
    mixin mixinOpAssign!"+";
    mixin mixinOpAssign!"-";
}


void main()
{

    T t;
    string s = t.x();
    assert(s == "x");
    assert(t.y == "y");

    //explicit call should work
    assert(t.opDispatch!"x" == "x");


    //TODO: fix these
    Arithmetic a;
    assert((a + 5) == 5);
    assert((a - 7) == -7);


    ArithmeticRight ar;
    assert((5 + ar) == 5);
    assert((7 - ar) == -7);


    U u;
    //should work for a single mixin
    assert(u.z == "z");

    TAssign ta;
    ta.x = 5;
    assert(ta.val == 5);
    ta.y = 10;
    assert(ta.val == 10);

    AssignOverloads oa;
    oa += 5;
    assert(oa.val == 5);
    assert(oa.lastOp == "+");
    oa -= 10;
    assert(oa.val == 10);
    assert(oa.lastOp == "-");

}
