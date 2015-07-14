/* { dg-do compile } */
/* { dg-options "-fstack-limit-symbol=__stackend -fno-pic" } */
/* { dg-final { scan-assembler "movhi\\t.*, %hiadj\\(__stackend.*\\)" } } */
/* { dg-final { scan-assembler "addi\\t.*, .*, %lo\\(__stackend.*\\)" } } */
/* { dg-final { scan-assembler "bgeu\\tsp, " } } */
/* { dg-final { scan-assembler "trap\\t3|trap.n\\t3" } } */

/* check stack checking */
void test()
{
  int a, b, c;
}
