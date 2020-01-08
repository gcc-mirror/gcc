// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct A {
};

int main()
{
  A a;
  typedef void (A::*F)();
  F p;

  const_cast<const A>(a); // { dg-error "3:invalid use of .const_cast." } const_cast requires pointer/ref types
  const_cast<F>(p); // { dg-error "3:invalid use of .const_cast." } const_cast requires pointer/ref types
  const_cast<int (*)()>(&main); // { dg-error "3:invalid use of .const_cast." } function type in const_cast
  // { dg-error "26:ISO C\\+\\+ forbids taking address" "" { target *-*-* } .-1 }
  const_cast<int (&)()>(main); // { dg-error "3:invalid use of .const_cast." } function type in const_cast
}
