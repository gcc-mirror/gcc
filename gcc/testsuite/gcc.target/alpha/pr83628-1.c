/* PR target/83628 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;

int
get_int (int *p, size_t idx)
{
  return p[idx];
}

long
get_long (long *p, size_t idx)
{
  return p[idx];
}

/* { dg-final { scan-assembler-not "\[ \t\]add\[ql\]" } } */
