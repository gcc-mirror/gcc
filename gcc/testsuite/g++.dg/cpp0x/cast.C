// I, Howard Hinnant, hereby place this code in the public domain.

// Test cast from lvalue to rvalue

// { dg-do compile }
// { dg-options "-std=c++0x" }

template <bool> struct sa;
template <> struct sa<true> {};

struct one   {char x[1];};
struct two   {char x[2];};

struct A {};

one foo(const A&) {return one();}
two foo(A&&)      {return two();}

int test1()
{
    A a;
    sa<sizeof(foo(a)) == 1> t1;
    sa<sizeof(foo(static_cast<A&&>(a))) == 2> t2;
    return 0;
}

int main()
{
    return test1();
}
