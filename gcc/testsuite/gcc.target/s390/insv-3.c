/* { dg-do compile } */
/* { dg-options "-O3 -march=z10 -mzarch" } */

/* risbg with z bit would work here but we rather want this to be a shift.  */
struct
{
  int a:31;
  int b:1;
} s;

void
foo (int in)
{
  s.a = in;
  s.b = 0;
}

/* { dg-final { scan-assembler-not "risbg" } } */
