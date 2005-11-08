// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-sources "static-1a.cc" }

extern "C" void abort ();
extern int test ();

struct A
{
  static __thread int i;
};

__thread int A::i = 8;

int
main ()
{
  if (A::i != 8)
    abort ();

  if (test ())
    abort ();

  if (A::i != 17)
    abort ();

  return 0;
}
