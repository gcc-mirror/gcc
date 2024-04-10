/* { dg-options "-std=gnu23 -Wno-div-by-zero" } */

enum e : bool { X };

enum e
f ()
{
  return 0 / 0;
}
