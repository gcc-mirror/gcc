// { dg-do run { target i?86-*-* } }
// { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } }
// Test that stdcall doesn't prevent us from using op delete.
// Contributed by Jason Merrill <jason@cygnus.com>

struct A {
  void operator delete (void *) __attribute__ ((stdcall));
};

void A::operator delete (void *) { }

int main()
{
  A* ap = new A;
  delete ap;
}
