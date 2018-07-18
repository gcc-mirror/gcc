/* PR target/79514 */
/* { dg-do compile } */
/* { dg-options "-m96bit-long-double" } */

extern void bar (long double);

extern long double x;

void foo (void)
{
  bar (x);
}
