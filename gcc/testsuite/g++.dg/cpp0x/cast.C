// I, Howard Hinnant, hereby place this code in the public domain.

// Test cast from lvalue to rvalue

// { dg-do compile }
// { dg-options "-std=c++0x" }
// { dg-skip-if "packed attribute missing for struct one" { "epiphany-*-*" } { "*" } { "" } }

template <bool> struct sa;
template <> struct sa<true> {};

struct one   {long x[1];};
struct two   {long x[2];};

struct A {};

one foo(const A&) {return one();}
two foo(A&&)      {return two();}

int test1()
{
    A a;
    sa<sizeof(foo(a)) == 1 * sizeof(long)> t1;
    sa<sizeof(foo(static_cast<A&&>(a))) == 2 * sizeof(long)> t2;
    return 0;
}

int main()
{
    return test1();
}
