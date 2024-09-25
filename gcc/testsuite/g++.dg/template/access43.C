// PR c++/116323
// { dg-do "compile" }
// { dg-additional-options "-Wno-template-body" }

class A { enum Enum{}; };

template<typename E, template<typename> class Alloc>
class B : private Alloc<E>, private A {};

template<typename E, template<typename> class Alloc>
int B<E, Alloc>::foo (Enum m) { return 42; } // { dg-error "is private" }
