/* { dg-do compile } */
/* { dg-options "-O0" } */

char a[1][1];
long long b;

void
foo (void)
{
  --a[b][b];
}
