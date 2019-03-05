/* PR target/89506 */
/* { dg-do compile } */
/* { dg-options "-Og -g -w" } */

long long a;
int c;

int
foo (long long d, short e)
{
  __builtin_sub_overflow (0xffffffff, c, &a);
  e >>= ~2147483647 != (int) a;
  return d + e;
}
