//PR c++/27667

struct A
{
    template<int> static void foo   () {}
    template<>    static void foo<0>() {}  // { dg-error "explicit" }
}; 
