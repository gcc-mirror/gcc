// PR c++/9282
// Origin: Thomas Richter <thor@math.tu-berlin.de>
// { dg-do compile }

typedef void (*fptr)();

struct A
{
    template<int>    static void foo() {}
    template<fptr f> static void bar() { (*f)(); }
};

fptr f = A::bar< A::foo<0> >;
