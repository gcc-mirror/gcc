// EXTRA_CPP_SOURCES: externmangle2.cpp
// DISABLED: win

extern(C++):

struct Test32NS1
{
    struct Foo(X)
    {
        X *v;
    }


    struct Bar(X)
    {
        X *v;
    }

};

struct Test32NS2
{
    struct Foo(X)
    {
        X *v;
    }
};

struct Test32(alias Y, alias Z)
{
    Y!(int)* field;
};


void test32a(Test32!(Test32NS1.Foo, Test32NS1.Foo) arg);
void test32b(Test32!(Test32NS1.Foo, Test32NS1.Bar) arg);
void test32c(Test32!(Test32NS1.Foo, Test32NS2.Foo) arg);
void test32d(Test32!(Test32NS1.Foo, Test32NS2.Foo) arg1, Test32!(Test32NS2.Foo, Test32NS1.Foo) arg2);

interface XXX
{
}

void test33a(XXX, XXX*);


struct Test33(alias A, alias B)
{
}

/*
void test33(XXX, Test33!(test33a, test33a) arg, XXX);


struct Test34(alias A)
{
};

struct Test34A
{
    static void foo(int);
};


void test34(Test34!(Test34A.foo) arg);
*/

__gshared extern int test36;

/*
struct Test37(alias A)
{
};

struct Test37A
{
    __gshared extern int t38;
};

void test37(Test37!(test36) arg);
void test38(Test37!(Test37A.t38) arg);
*/

struct Test39
{
    struct T39A(X)
    {
    }
}

struct T39A
{
}

void test39(Test39.T39A!(.T39A));

version(none)
{
    version(Posix) //Only for g++ with -std=c++0x and Visual Studio 2013+
    {

        struct Test40(T, V...)
        {

        }

        void test40(Test40!(int, double, void))
        {
        }
    }
    else version(Win64) //Only for g++ with -std=c++0x and Visual Studio 2013+
    {

        struct Test40(T, V...)
        {

        }

        void test40(Test40!(int, double, void))
        {
        }
    }
}


__gshared extern const XXX test41;
struct Test42
{
    __gshared extern const XXX test42;
}
__gshared extern int[4] test43;
const(XXX) test44();

void main()
{
    test32a(Test32!(Test32NS1.Foo, Test32NS1.Foo)());
    test32b(Test32!(Test32NS1.Foo, Test32NS1.Bar)());
    test32c(Test32!(Test32NS1.Foo, Test32NS2.Foo)());
    test32d(Test32!(Test32NS1.Foo, Test32NS2.Foo)(), Test32!(Test32NS2.Foo, Test32NS1.Foo)());

    //test33a(null, null);
    //test33(null, Test33!(test33a, test33a)(), null);

    //test34(Test34!(Test34A.foo)());

    assert(test36 == 36);

    //test37(Test37!(test36)());
    //test38(Test37!(Test37A.t38)());
    test39(Test39.T39A!(.T39A)());

    assert(test41 is null);
    assert(Test42.test42 is null);
    assert(test43 == [1, 2, 3, 4]);
    auto ptr = &test44;
}
