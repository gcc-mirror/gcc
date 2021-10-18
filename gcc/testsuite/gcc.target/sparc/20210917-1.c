/* PR rtl-optimization/102306 */
/* Reported by Daniel Cederman <cederman@gaisler.com> */

/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O -mcpu=v8" } */

extern void foo (void);

void test (volatile unsigned char *a) 
{ 
  char b = *a;
  if (!b)
    return;
  if (b & 2)
    foo ();
}

/* { dg-final { scan-assembler-times "ldub" 1 } } */
