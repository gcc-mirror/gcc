/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2" } */

typedef unsigned int UTItype __attribute__ ((mode (TI)));

void foo (UTItype *);

UTItype
test (void)
{
  UTItype c = 0;
  foo (&c);
  c = c >> 5 | c << 123;
  return c;
}
/* { dg-final { scan-assembler-times "shrdq" 2 } } */
