/* { dg-do compile } */
/* { dg-options "-Wenum-compare" } */
enum E1 { A, B, C };
enum E2 { D, E, F };
extern void f2 ();
void
f1 ()
{
  int a = A;
  int d = D;
  enum E1 e1 = A;
  enum E2 e2 = D;
  if (A > D)	/* { dg-warning "comparison between .enum E1. and .enum E2." } */
    f2 ();
  if (e1 > e2)  /* { dg-warning "comparison between .enum E1. and .enum E2." } */
    f2 ();
  if (e1 > e2 + 1)
    f2 ();
  if (A > 0)
    f2 ();
  if (e1 > 0)
    f2 ();
  if (A + D > 0)
    f2 ();
  if (e1 > 0)
    f2 ();
  if (A + D > 0)
    f2 ();
  if ((int) A > D)
    f2 ();
  if ((int) e1 > e2)
    f2 ();
}
