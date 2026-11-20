/* PR tree-optimization/128869 */
/* { dg-do run { target bitint } } */
/* { dg-options "-O1" } */

[[gnu::noipa]] int
foo (unsigned _BitInt(1) a, unsigned _BitInt(1) c)
{
#if __BITINT_MAXWIDTH__ >= 257
  signed _BitInt(257) s = c, y;
  switch (s)
    {
    case 14: y = a; break;
    case 0x1e768b9a0533c4b55uwb: y = 5; break;
    default: y = 0; break;
    }
  signed _BitInt(255) r;
  /* 2^255 - 0 does not fit in signed _BitInt(255): must report overflow.  */
  return __builtin_sub_overflow ((unsigned _BitInt(256)) 1 << 255, y, &r);
#else
  return 1;
#endif
}

[[gnu::noipa]] int
bar (unsigned _BitInt(1) a, unsigned _BitInt(1) c)
{
#if __BITINT_MAXWIDTH__ >= 257
  signed _BitInt(257) s = c, y;
  switch (s)
    {
    case 14: y = a; break;
    case 0x1e768b9a0533c4b55uwb: y = 5; break;
    default: y = 0; break;
    }
  signed _BitInt(127) r;
  /* 2^127 - 0 does not fit in signed _BitInt(127): must report overflow.  */
  return __builtin_sub_overflow ((unsigned _BitInt(128)) 1 << 127, y, &r);
#else
  return 1;
#endif
}

int
main ()
{
  if (foo (0, 0) != 1 || bar (0, 0) != 1)
    __builtin_abort ();
}
