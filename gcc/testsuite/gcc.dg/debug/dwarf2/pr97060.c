/* PR debug/97060 */
/* { dg-do compile } */
/* { dg-options "-g -dA" } */
/* { dg-final { scan-assembler-times "DW_AT_declaration" 2 } } */

extern int foo (unsigned int, unsigned int);

int
bar (void)
{
  foo (1, 2);
  return 0;
}
