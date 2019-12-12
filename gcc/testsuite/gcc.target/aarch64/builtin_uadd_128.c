/* { dg-do compile } */
/* { dg-options "-O2" }  */

extern void overflow_handler ();

unsigned __int128 overflow_add (unsigned __int128 x, unsigned __int128 y)
{
  unsigned __int128 r;

  int ovr = __builtin_add_overflow (x, y, &r);
  if (ovr)
    overflow_handler ();

  return r;
}

/* { dg-final { scan-assembler "adds" } } */
/* { dg-final { scan-assembler "adcs" } } */
