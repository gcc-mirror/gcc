/* { dg-do compile { target dfp } } */
/* { dg-options "-O -Wno-psabi" } */

typedef _Float64 __attribute__((__vector_size__ (32))) F;
typedef _Decimal32 __attribute__((__vector_size__ (16))) D;

extern void bar (void);

D g;
void
foo (F f)
{
  D d = __builtin_convertvector (f, D);
  bar ();
  g = d;
}
