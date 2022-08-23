/* PR rtl-optimization/105333 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Og -fno-tree-coalesce-vars -fno-tree-fre" } */

int g;
short s;

static inline unsigned short
bar (short a, __int128 b)
{
  b ^= (unsigned long) -a;
  __builtin_strncpy ((void *) &s, (void *) &a, 1);
  b *= 14;
  return b;
}

void
foo (void)
{
  g *= (__int128) bar (1, 1);
}
