/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=gnu23 -w" } */

_BitInt(37)
foo (_BitInt(37) x, _BitInt(37) y)
{
  _BitInt(37) w;
  __asm ("# %0 %1 %2 %3" : "=r" (w) : "r" (x), "r" (x + y), "g" (68719476735wb));
  return w;
}

#if __BITINT_MAXWIDTH__ >= 125
_BitInt(125)
bar (_BitInt(125) x, _BitInt(125) y)
{
  _BitInt(125) w;
  __asm ("# %0 %1 %2 %3" : "=g" (w) : "g" (x), "g" (x + y), "g" (21267647932558653966460912964485513215wb));
  return w;
}
#endif

#if __BITINT_MAXWIDTH__ >= 575
_BitInt(575)
baz (_BitInt(575) x, _BitInt(575) y)
{
  _BitInt(575) w;
  __asm ("# %0 %1 %2 %3" : "=g" (w) : "g" (x), "g" (x + y), "g" (61832600368276133515125630254911797508782837275302959978515764023224306276632966792579100265310761247399417856504034834837841258576687802491886538775473291979151693037174783wb));
  return w;
}
#endif
