// { dg-do run }
// { dg-require-effective-target tls }
// { dg-add-options tls }
// { dg-additional-sources "static-1a.mm" }

extern "C" {
extern void abort ();
}
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
