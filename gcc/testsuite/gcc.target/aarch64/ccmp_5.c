
/* { dg-options "-O2" } */
/* PR target/100942 */
void f1(int a, int b, _Bool *x)
{
  x[0] = x[1] = a == 0 || b == 0;
}

void f2(int a, int b, int *x)
{
  x[0] = x[1] = a == 0 || b == 0;
}


/* Both functions should be using ccmp rather than 2 cset/orr.  */
/* { dg-final { scan-assembler-times "\tccmp\t" 2 } } */
/* { dg-final { scan-assembler-times "\tcset\t" 2 } } */
/* { dg-final { scan-assembler-times "\tcmp\t" 2 } } */
/* { dg-final { scan-assembler-not "\torr\t" } } */

