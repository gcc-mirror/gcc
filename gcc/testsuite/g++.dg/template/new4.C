// PR c++/27559
// { dg-do compile }

struct A
{
    template<typename T>
    static void* operator new(T) {} // { dg-error "first parameter|invalid template" }
};
