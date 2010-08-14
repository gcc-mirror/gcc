// I, Howard Hinnant, hereby place this code in the public domain.

// Test: Unamed rvalue references are treated as lvalues.

// { dg-do compile }
// { dg-options "-std=c++0x" }

template <bool> struct sa;
template <> struct sa<true> {};

struct one   {long x[1];};
struct two   {long x[2];};

struct A {};

one foo(const A&) {return one();}
two foo(A&&)      {return two();}

template<typename _Tp>
inline _Tp&&
movel(_Tp& __t)
{ return static_cast<_Tp&&>(__t); }

A&& source() {static A a; return movel(a);}

int test1()
{
    sa<sizeof(foo(source())) == 2 * sizeof(long)> t1;
    return 0;
}

int main()
{
    return test1();
}
