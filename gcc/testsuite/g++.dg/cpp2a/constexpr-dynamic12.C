// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++20 } }

// dynamic_cast in a destructor.

struct A2 { virtual void a2(); };

struct A : A2 { virtual void a(); };

struct C2 { virtual void c2(); };

struct B : A, C2 {
  constexpr ~B();
};

constexpr B::~B()
{
  A *a = dynamic_cast<A*>((C2*)this);
  if (a != (A*) this)
    __builtin_abort ();
  A& ar = dynamic_cast<A&>((C2&)*this);
  if (&ar != &(A&)*this)
    __builtin_abort ();
}

struct D : B { virtual void d(); };

constexpr D d;
