// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

// dynamic_cast in a constructor.
// [class.cdtor]#6: "If the operand of the dynamic_cast refers to the object
// under construction or destruction and the static type of the operand is not
// a pointer to or object of the constructor or destructor's own class or one
// of its bases, the dynamic_cast results in undefined behavior.
// See <https://gcc.gnu.org/ml/gcc-patches/2019-12/msg01521.html>.

struct V {
  virtual void f();
};

struct A : V { };

struct B : V {
  constexpr B(V*, A*);
};

struct D : B, A {
  constexpr D() : B((A*)this, this) { } // { dg-message "in 'constexpr' expansion of" }
};

constexpr B::B(V* v, A* a)
{
  dynamic_cast<B*>(a); // { dg-error "accessing uninitialized member" }
}

constexpr D d; // { dg-message "in 'constexpr' expansion of" }
