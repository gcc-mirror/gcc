/* { dg-do compile } */
/* { dg-options " " } */
/* { dg-final { scan-assembler-not "trap\\t3|trap.n\\t3" } } */

/* check stack checking */
void test()
{
  int a, b, c;
}
