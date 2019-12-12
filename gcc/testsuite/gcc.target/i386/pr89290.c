/* PR target/89290 */
/* { dg-do compile { target { tls && lp64 } } } */
/* { dg-options "-O0 -mcmodel=large" } */

struct S { long int a, b; } e;
__thread struct S s;
__thread struct S t[2];

void
foo (void)
{
  s = e;
}

void
bar (void)
{
  t[1] = e;
}
