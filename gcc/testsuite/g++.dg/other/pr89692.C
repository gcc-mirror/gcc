// PR lto/89692
// { dg-do compile }
// { dg-require-effective-target lto }
// { dg-options "-flto -O2" }

struct S {
  short int a, b;
  unsigned char c : 1;
};

bool
foo (void)
{
  unsigned char d[sizeof (S)] = { 0 };
  S e;

  __builtin_memcpy (&e, d, sizeof (d));

  return e.c == d[0];
}
