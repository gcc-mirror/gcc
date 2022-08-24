/* PR target/105214 */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-Ofast -fnon-call-exceptions -fexceptions -fstack-check=generic -fsanitize=address -fno-finite-math-only -fsignaling-nans -fno-associative-math" } */

float f;
void bar (int *);

void
foo (void)
{
  int a[1600], b[1];
  f += __builtin_log1pf (f);
  bar (a);
  bar (b);
}
