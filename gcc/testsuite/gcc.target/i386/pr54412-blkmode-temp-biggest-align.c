/* PR target/54412 */
/* Check that "assign_stack_temp_for_type" does not overstate the alignment
   of a "BLKmode" temporary.  */
/* { dg-do compile { target x86_64-*-mingw* } } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler-not {vmov(aps|dqa)\t%ymm[0-9]+, [0-9-]*\(%r[sb]p\)} } } */

struct S { char c[32]; };

void sink (struct S);

void
caller (struct S *p)
{
  sink (*p);
}
