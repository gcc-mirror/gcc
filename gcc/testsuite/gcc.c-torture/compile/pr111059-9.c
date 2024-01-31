/* { dg-options "-std=gnu23 -Wno-shift-count-negative" } */

enum e : bool { X };

enum e
f ()
{
  return 1 << -1;
}
