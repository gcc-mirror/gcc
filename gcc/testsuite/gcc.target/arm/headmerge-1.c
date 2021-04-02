/* { dg-do compile }  */
/* { dg-options "-O2 -mno-long-calls" }  */
/* { dg-final { scan-assembler-times "#120" 1 } } */

extern void foo1 (int);
extern void foo2 (int);

void t (int x, int y)
{
  if (y < 5)
    foo1 (120);
  else
    foo2 (120);
}
