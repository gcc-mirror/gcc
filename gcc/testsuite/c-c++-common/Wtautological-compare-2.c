/* PR bootstrap/67030 */
/* { dg-do compile } */
/* { dg-options "-Wtautological-compare" } */

extern int foo (void);

#define A a
#define B A
#define FOO (A > B)

void
fn1 (int a)
{
  if (FOO);
}
