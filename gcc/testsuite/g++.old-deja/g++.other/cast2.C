// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct A {
};

int main()
{
  A a;
  typedef void (A::*F)();
  F p;

  const_cast<const A>(a); // { dg-error "" } const_cast requires pointer/ref types
  const_cast<F>(p); // { dg-error "" } const_cast requires pointer/ref types
  const_cast<int (*)()>(&main); // { dg-error "" } function type in const_cast
  const_cast<int (&)()>(main); // { dg-error "" } function type in const_cast
}
