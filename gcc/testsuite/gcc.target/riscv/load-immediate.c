/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

/* Check that we don't have unnecessary load immediate instructions.  */
void
sub1 (int *a, long long *b)
{
  *a = 1;
  *b = 1;
}

void
sub2 (short *a, short *b)
{
  *a = -32768;
  *b = 32767;
}

void
sub3 (int *a, long long *b)
{
  *a = 10000;
  *b = 10000;
}

void
sub4 (int *a, short *b)
{
  *a = 1;
  *b = 1;
}
/* { dg-final { scan-assembler-times "\tli\t" 4 } } */
