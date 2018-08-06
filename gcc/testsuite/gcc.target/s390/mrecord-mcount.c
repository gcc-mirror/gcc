/* { dg-do compile } */
/* { dg-options "-pg -mrecord-mcount" } */

void
profileme (void)
{
  /* { dg-final { scan-assembler ".section __mcount_loc, \"a\",@progbits" } } */
  /* { dg-final { scan-assembler ".quad 1b" } } */
  /* { dg-final { scan-assembler ".previous" } } */
}
