/* PR target/11877 */
/* { dg-do compile } */
/* { dg-options "-Os" } */

void foo (long long *p)
{
  *p = 0;
}

void bar (int *p)
{
  *p = 0;
}

/* { dg-final { scan-assembler-times "xorl\[ \t\]" 2 } } */
/* { dg-final { scan-assembler-not "\\\$0," } } */
