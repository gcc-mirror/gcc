// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-require-effective-target ilp32 }
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
