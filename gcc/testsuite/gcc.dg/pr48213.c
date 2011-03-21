/* PR target/48213 */
/* { dg-do compile } */
/* { dg-options "-g -O2" } */
/* { dg-options "-g -O2 -fpic" { target fpic } } */

struct S { int (*s) (void); };
int bar (void);

void
foo (struct S *x)
{
  if (x->s != bar)
    bar ();
}
