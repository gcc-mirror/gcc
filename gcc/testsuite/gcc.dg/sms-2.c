/* The same test as 921011-1.c.  It can fails on gcc 4.1 due to a dfs
   traversal of the loops after versioning.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fmodulo-sched -fdump-rtl-sms" } */


void
fun (nb)
     int nb;
{
  int th, h, em, nlwm, nlwS, nlw, sy;

  while (nb--)
    while (h--)
      {
	nlw = nlwm;
	while (nlw)
	  {
	    if (nlwS == 1)
	      {
	      }
	    else
	      if (nlwS == 1)
		{
		}
	    nlwS--; nlw--;
	  }
	if (em)
	  nlwS--;
	if (++sy == th)
	  sy = 0;
      }
}

/* { dg-final { scan-rtl-dump-times "SMS succeeded" 1 "sms" { target spu-*-* powerpc*-*-* } } } */
/* { dg-final { cleanup-rtl-dump "sms" } } */
