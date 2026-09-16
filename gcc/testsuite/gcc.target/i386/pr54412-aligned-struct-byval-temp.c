/* PR target/54412 */
/* Check that "assign_stack_temp_for_type" preserves the required alignment
   of a caller-created temporary for a by-reference argument.  */
/* { dg-do compile { target x86_64-*-mingw* } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler {and[lq]?\t\$-32,} } } */

struct S { char c[32]; } __attribute__((aligned (32)));

void sink (struct S);

void
caller (struct S *p)
{
  sink (*p);
}
