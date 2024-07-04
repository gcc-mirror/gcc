/* { dg-options "-std=gnu23 -Wno-div-by-zero" } */

enum e : bool { X };

void
f ()
{
  (enum e) (0 / 0);
}
