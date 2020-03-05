/* PR rtl-optimization/90756 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi" } */
/* { dg-additional-options "-mno-sse" { target ia32 } } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

typedef float B __attribute__((vector_size(4 * sizeof (float))));
typedef unsigned long long C __attribute__((vector_size(4 * sizeof (long long))));
typedef short D __attribute__((vector_size(4 * sizeof (short))));
B z;
void foo (C);
C bar (D);
B baz ();
D qux (B);

void
quux (int x)
{
  B n = z, b = z;
  while (1)
    switch (x)
      {
      case 0: n = baz (); /* FALLTHRU */
      case 1: { B o = n; n = b; b = o; } /* FALLTHRU */
      case 2: { D u = qux (b); C v = bar (u); foo (v); }
      }
}
