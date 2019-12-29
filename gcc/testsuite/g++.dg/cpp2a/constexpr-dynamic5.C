// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++2a } }

// Multiple levels.

struct A { virtual void a(); };
struct B : A { virtual void b(); };
struct C : B { virtual void c(); };
struct D : C { virtual void d(); };
struct E : D { virtual void e(); };
struct F : E { virtual void f(); };

constexpr F f;

// F->C->A->B == F->C->B
static_assert (&dynamic_cast<B&>((A&)(C&)f) == &(B&)(C&)f);
// F->A->E == F->E
static_assert (&dynamic_cast<E&>((A&)f) == &(E&)f);
// F->E->D->C->B->A->C == F->C
static_assert (&dynamic_cast<C&>((A&)(B&)(C&)(D&)(E&)f) == &(C&)f);
// F->B->F == F
static_assert (&dynamic_cast<F&>((B&)f) == &f);
