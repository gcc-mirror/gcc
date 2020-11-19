/* { dg-do compile } */
/* { dg-options "" } */

void h ();
void f () __attribute__ ((returns_twice));
void g (_Complex int a)
{
  f ();
  if (a != 0)
  {
    a = 0;
    h ();
  }
}
