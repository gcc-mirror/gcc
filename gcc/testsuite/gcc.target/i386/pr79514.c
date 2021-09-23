/* PR target/79514 */
/* { dg-do compile } */
/* { dg-options "-m96bit-long-double" } */
/* { dg-error "'-m96bit-long-double' is not compatible" "" { target { ! ia32 } } 0 } */

extern void bar (long double);

extern long double x;

void foo (void)
{
  bar (x);
}
