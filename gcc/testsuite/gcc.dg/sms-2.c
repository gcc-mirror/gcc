/* The same test as 921011-1.c.  It can fails on gcc 4.1 due to a dfs
   traversal of the loops after versioning.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fmodulo-sched -fdump-rtl-sms" } */

int th, h, em, nlwm, nlwS, nlw, sy;
void
fun (nb)
     int nb;
{

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

/* { dg-final { scan-rtl-dump-times "SMS loop many exits" 1 "sms" { target spu-*-* powerpc*-*-* } } } */
/* { dg-final { cleanup-rtl-dump "sms" } } */
