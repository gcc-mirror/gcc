// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct A {
};

int main()
{
  A a;
  const_cast<const A>(a); // ERROR - const_cast requires pointer/ref types
}
