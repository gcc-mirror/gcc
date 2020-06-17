/* { dg-do compile } */
/* { dg-options "-O2 -flto -maltivec -mno-vsx" } */
/* { dg-require-effective-target lto } */
/* { dg-require-effective-target powerpc_altivec_ok } */

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
