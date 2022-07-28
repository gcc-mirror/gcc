// REQUIRED_ARGS: -w
// PERMUTE_ARGS:

extern(C) int printf(const char*, ...);

/******************************************/

class F { }
int foo()
{
    scope F f = new F(); // comment out and warning goes away
    return 0;
}

/******************************************/

int foo2()
{
    try
    {
        return 0;
    }
    finally { }
}

/******************************************/

private int getNthInt(A...)(uint index, A args)
{
    foreach (i, arg; args)
    {
        static if (is(typeof(arg) : long) || is(typeof(arg) : ulong))
        {
            if (i != index) continue;
            return cast(int)(arg);
        }
        else
        {
            if (i == index) break;
        }
    }
    throw new Error("int expected");
}

/******************************************/

class Foo2 {
    int foo() {
        synchronized(this){
            return 8;
        }
    }
}

/******************************************/

void mainX()
{
    int i=0;
    printf("This number is zero: ");
    goto inloop;
    for(; i<10; i++) {              // this is line 7
        printf("This number is nonzero: ");
inloop:
        printf("%d\n", i);
    }
}

/******************************************/

string foo3(int bar)
{
    switch (bar)
    {
        case 1:
            return "1";
        case 2:
            return "2";
        default:
            return "3";
    }
}


/******************************************/

int foo4()
{
    int i;
    for (;; ++i)
    {
        if (i == 10) return 0;
    }
}

/******************************************/

int foo5()
{
    do {
        if (false)
            return 1;
    } while (true);
}

/******************************************/

nothrow int foo6()
{
    int x = 2;
    try
    {
    }
    catch(Exception e)
    {
        x = 4;
        throw new Exception("xxx");
    }
    return x;
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=6518

template TypeTuple(T...) { alias T TypeTuple; }
void test6518()
{
    switch(2)
    {
        foreach(v; TypeTuple!(1, 2, 3))
            case v: break;
        default: assert(0);
    }
}

/***************************************************/

struct S9332
{
    this(S9332)
    {
//      while (1) { }
        assert(0, "unreachable?");
    }
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13201

class C13201
{
    void foo()
    {
        synchronized(this)
        {
            assert(0);
        }
    }
}

void test13201a()
{
    auto c = new C13201();
    synchronized(c)
    {
        assert(0);
    }
}

void test13201b()
{
    struct S { ~this() {} }

    S s;
    assert(0);
}

/******************************************/

void main()
{
}
