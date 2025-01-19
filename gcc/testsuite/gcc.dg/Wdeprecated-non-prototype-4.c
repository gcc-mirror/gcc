/* { dg-do compile } */
/* { dg-options "-std=gnu17 -Wc11-c23-compat -Wno-deprecated-non-prototype" } */

void f1 ();
void f2 ();

void
g ()
{
  f1 ();
  f2 (1);
}

void
f1 ()
{
}

void
f2 (int i)
{
}
