/* { dg-options "-std=gnu23 -Wno-overflow" } */

enum e : bool { X };

enum e
f ()
{
  return __INT_MAX__ + 1;
}
