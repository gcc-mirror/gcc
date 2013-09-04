/* PR 23190 */
/* { dg-do compile }
/* { dg-options "-O2 -gdwarf -dA" } */
/* { dg-final { scan-assembler "DW_OP_addr\[\\n\\r\]+\[^\\n\\r\]+foo" } } */
/* { dg-final { scan-assembler "DW_OP_addr\[\\n\\r\]+\[^\\n\\r\]+bar" } } */

static int foo;
int bar;
int main(void)
{
   foo += 3;
   bar *= 5;
   return 0;
}
