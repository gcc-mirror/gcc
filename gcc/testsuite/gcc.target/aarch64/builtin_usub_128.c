/* { dg-do compile } */
/* { dg-options "-O2" }  */

extern void overflow_handler ();

unsigned __int128 overflow_sub (unsigned __int128 x, unsigned __int128 y)
{
  unsigned __int128 r;

  int ovr = __builtin_sub_overflow (x, y, &r);
  if (ovr)
    overflow_handler ();

  return r;
}

/* { dg-final { scan-assembler "subs" } } */
/* { dg-final { scan-assembler "sbcs" } } */
