// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++2a } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

// dynamic_cast in a constructor.
// [class.cdtor]#6: "If the operand of the dynamic_cast refers to the object
// under construction or destruction and the static type of the operand is not
// a pointer to or object of the constructor or destructor's own class or one
// of its bases, the dynamic_cast results in undefined behavior.

struct V {
  virtual void f();
};

struct A : V { };

struct B : V {
  constexpr B(V*, A*);
};

struct D : A, B {
  constexpr D() : B((A*)this, this) { } // { dg-message "in 'constexpr' expansion of" }
};

constexpr B::B(V* v, A* a)
{
  // well-defined: v of type V*, V base of B results in B*
  B* b = dynamic_cast<B*>(v);
  if (b != nullptr)
    __builtin_abort ();

  B& br = dynamic_cast<B&>(*v); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "dynamic type .A. of its operand does not have an unambiguous public base class .B." "" { target *-*-* } .-1 }
}

constexpr D d; // { dg-message "in 'constexpr' expansion of" }
