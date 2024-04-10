/* { dg-options "-std=gnu23 -Wno-overflow" } */

enum e : bool { X };

void
f ()
{
  (enum e) (__INT_MAX__ + 1);
}
