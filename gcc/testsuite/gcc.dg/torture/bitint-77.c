/* PR tree-optimization/119722 */
/* { dg-do run { target bitint } } */
/* { dg-options "-O2 -fno-tree-forwprop -fno-tree-copy-prop -fno-tree-fre" } */

#if __BITINT_MAXWIDTH__ >= 33300
unsigned _BitInt(33300) g;

unsigned
foo (long c)
{
  unsigned _BitInt(33300) b
    = __builtin_stdc_rotate_left ((unsigned _BitInt(13)) 8, c);
  return ((unsigned _BitInt(50)) (g >> 50)
	  + ({ unsigned _BitInt(300) unused; b; }));
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 33300
  unsigned  x = foo (0);
  if (x != 8)
    __builtin_abort ();
#endif
}
