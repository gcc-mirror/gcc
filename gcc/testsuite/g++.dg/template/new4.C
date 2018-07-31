// PR c++/27559
// { dg-do compile }

struct A
{
    template<typename T>
    static void* operator new(T) {}  // { dg-error "invalid template" }
// { dg-error "18:.operator new. takes type .size_t." "first" { target *-*-* } .-1 }
};
