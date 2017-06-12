/* { dg-do compile } */
/* { dg-options "-O3" } */

void
a ()
{
  unsigned _Accum b;
  for (b = 0.1; b; b += 0.1uk)
    {
      _Sat unsigned _Accum b;
      for (b = 0; b <= 0.8; b = 0.1)
	;
    }
}
