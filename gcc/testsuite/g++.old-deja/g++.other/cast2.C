// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct A {
};

int main()
{
  A a;
  typedef void (A::*F)();
  F p;

  const_cast<const A>(a); // ERROR - const_cast requires pointer/ref types
  const_cast<F>(p); // ERROR - const_cast requires pointer/ref types
  const_cast<int (*)()>(&main); // ERROR - function type in const_cast
  const_cast<int (&)()>(main); // ERROR - function type in const_cast
}
