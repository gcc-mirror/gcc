string result;

struct A
{
    this(ref A rhs)
    {
        result ~= "A";
    }
    this(ref immutable A rhs)
    {
        result ~= "B";
    }
    this(ref const A rhs)
    {
        result ~= "C";
    }
    this(ref A rhs) immutable
    {
        result ~= "D";
    }
    this(ref const A rhs) shared
    {
        result ~= "E";
    }
    this(ref A rhs) shared
    {
        result ~= "F";
    }
    this(ref shared A rhs) immutable
    {
        result ~= "G";
    }
    this(ref shared A rhs) shared
    {
        result ~= "H";
    }
}

// copy constructor correctly uses function declaration overload resolution
void test1()
{
    result = "";
    A a;
    A a1 = a;
    immutable A a2 = a;
    const A a3 = a2;
    shared A a4 = a3;
    A a5 = a3;
    assert(result == "ADBEC");
}

// copy constructor has priority over alias this
struct B
{
    B fun(immutable B)
    {
        return B();
    }

    this(ref immutable B rhs)
    {
        result ~= "A";
    }
    alias fun this;
}

void test2()
{
    result = "";
    immutable B a;
    B a1 = a;
    assert(result == "A");
}

// arguments and return values correctly call the copy constructor
shared(A) fun(A x)
{
    return x;
}

immutable(A) fun2(shared A x)
{
    return x;
}

void test3()
{
    result = "";
    A a1;
    shared A a2 = fun(a1);
    immutable A a3 = fun2(a2);
    assert(result == "AFHG");
}

// nested structs
int fun()
{
    int x = 1;
    struct A
    {
        struct B
        {
            int x2 = 2;
        }

        B b;
        int x1;

        this(int x)
        {
            this.x1 = x;
            b = B(3);
        }
        this(ref A rhs)
        {
            this.x1 = rhs.x1 + rhs.b.x2 + x;
        }
    }

    A a = A(2);
    A b = a;
    return b.x1;
}

void test4()
{
    assert(fun() == 6);
}

// generated copy constructor
struct X
{
    this(ref inout(X) rhs) inout
    {
        result ~= "A";
    }
}

struct Y
{
    X a;
}

void test5()
{
    result = "";
    Y b1;
    Y b2 = b1;
    assert(result == "A");
}

void main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
}
