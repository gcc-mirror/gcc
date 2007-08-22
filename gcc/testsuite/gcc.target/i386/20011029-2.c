/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo (int s)
{
  for (;;)
    {
      int a[32];
      int y, z;
      __asm__ __volatile__ ("" : "=c" (y), "=D" (z)
			    : "a" (0), "0" (32), "1" (a) : "memory");
      if (({ register char r;
	     __asm__ __volatile__ ("" : "=q" (r)
				   : "r" (s % 32), "m" (a[s / 32])
				   : "cc"); r; }))
        continue;
      else if (({ register char r;
		  __asm__ __volatile__ ("" : "=q" (r)
					: "r" (0), "m" (a[0])
					: "cc"); r; }))
        continue;
    }
  return 0;
}
