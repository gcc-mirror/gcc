/* PR target/88594 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fno-tree-forwprop -fno-tree-vrp" } */

__int128
foo (__int128 x, __int128 *y)
{
  int a;
  __int128 z, r;
  for (a = 0; a < 17; ++a)
    ;
  z = x / a;
  r = x % a;
  *y = z;
  return r;
}
