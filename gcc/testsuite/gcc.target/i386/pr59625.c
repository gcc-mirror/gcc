/* PR target/59625 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=atom" } */

int
foo (void)
{
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  asm goto ("" : : : : lab);
  return 0;
lab:
  return 1;
}

/* Verify we don't consider asm goto as a jump for four jumps limit
   optimization.  asm goto doesn't have to contain a jump at all,
   the branching to labels can happen through different means.  */
/* { dg-final { scan-assembler-not "(p2align\[^\n\r\]*\[\n\r]*\[^\n\r\]*){8}p2align" } } */
