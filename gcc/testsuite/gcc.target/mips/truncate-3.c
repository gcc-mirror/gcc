/* Remove redundant operations in truncate's operand.  */
/* { dg-options "-mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\tandi?\t" } } */

f (long long d)
{
  long long c = d & 0xffffffffff;
  int i = (int) c;
  g (i);
}

