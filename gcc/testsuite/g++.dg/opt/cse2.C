// This testcase caused ICE on IA-32 in simplify_unary_operation
// CSE did not assume SUBREGs changing mode from integral to floating.
// { dg-do run { target i?86-*-* sparc*-*-* } }
// { dg-options "-O2" }

struct A
{
  union
    {
      float f;
      unsigned int w;
    } a;

  static inline const A foo (void)
    {
      return A ((unsigned int) (__extension__ ((union { unsigned l; float d; })
					       { l: 0x3f800000 }).d));
    }
  inline A (float f) { a.f = f; }
  A ();
  inline A (unsigned int w) { a.w = w; }
};

A::A()
{
  *this = foo ();
}

A a;

extern "C" void abort (void);
extern "C" void exit (int);

int main ()
{
  if (a.a.w != 1)
    abort ();
  exit (0);
}
