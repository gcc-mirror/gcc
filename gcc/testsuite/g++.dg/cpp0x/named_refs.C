// I, Howard Hinnant, hereby place this code in the public domain.

// Test: Named rvalue references are treated as lvalues.

// { dg-do compile { target c++11 } }
// { dg-skip-if "packed attribute missing for struct one" { "epiphany-*-*" } }

template <bool> struct sa;
template <> struct sa<true> {};

struct one   {long x[1];};
struct two   {long x[2];};

struct A {};

one foo(const A&) {return one();}
two foo(A&&)      {return two();}

int test1(A&& a)
{
    sa<sizeof(foo(a)) == 1 * sizeof(long)> t1;
    return 0;
}

int main()
{
    return test1(A());
}
