/* Remove redundant operations in truncate's operand.  */
/* { dg-options "-O -mgp64" } */
/* { dg-final { scan-assembler-not "\tandi?\t" } } */

f (long long d)
{
  long long c = d & 0xffffffffff;
  int i = (int) c;
  g (i);
}

