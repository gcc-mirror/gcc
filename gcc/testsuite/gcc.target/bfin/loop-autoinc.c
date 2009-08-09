/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler " = \\\[\[PI\].\\+\\+\\\];" } } */
extern int x[];
extern void bar();
int foo ()
{
  int i;
  int sum = 0;
  for (i = 0; i < 100; i++) {
    sum += x[i];
    if (sum & 1)
      sum *= sum;
  }
  return sum;
}
