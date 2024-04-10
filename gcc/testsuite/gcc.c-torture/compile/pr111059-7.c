/* { dg-options "-std=gnu23 -Wno-shift-count-negative" } */

enum e : bool { X };

void
f ()
{
  (enum e) (1 << -1);
}
