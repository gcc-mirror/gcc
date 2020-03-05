/* { dg-do compile } */
/* { dg-options "-O2 -mno-vsx" } */

vector int c, a, b;

static inline void __attribute__ ((__always_inline__, target ("no-vsx")))
foo ()
{
  c = a + b;
}

int
main ()
{
  foo ();
  c = a + b;
}
