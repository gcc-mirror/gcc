/* PR tree-optimization/128003 */
/* { dg-do run { target bitint } } */
/* { dg-options "-fwrapv" } */

#if __BITINT_MAXWIDTH__ >= 128
volatile int v;

[[gnu::noipa]] void
foo (_BitInt(128) x)
{
  switch (x)
    {
    case 10: v = 10; break;
    case 11: v = 11; break;
    case 12: v = 12; break;
    case 13: v = 13; break;
    case 14: v = 14; break;
    case 15: v = 15; break;
    case 16: v = 16; break;
    case 17: v = 17; break;
    default: v = -1; break;
    }
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 128
  foo (-18446744073709551605wb);
  if (v != -1)
    __builtin_abort ();
#endif
}
