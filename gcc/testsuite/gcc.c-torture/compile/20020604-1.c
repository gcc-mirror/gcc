/* { dg-do assemble } */
/* { dg-skip-if "The array is too big" { "avr-*-*" "pdp11-*-*" } { "*" } { "" } } */ 
/* { dg-xfail-if "The array too big" { "h8300-*-*" } { "-mno-h" "-mn" } { "" } } */
/* { dg-skip-if "" { m32c-*-* } { } { } } */

/* PR c/6957
   This testcase ICEd at -O2 on IA-32, because
   (insn 141 139 142 (set (subreg:SF (reg:QI 72) 0)
	   (plus:SF (reg:SF 73)
	       (reg:SF 76))) 525 {*fop_sf_comm_nosse} (insn_list 134 (nil))
       (expr_list:REG_DEAD (reg:SF 73) (nil)))
   couldn't be reloaded. */

void
foo (unsigned int n, int x, int y, unsigned char *z)
{
  int a, b;
  float c[2048][4];

  switch (x)
    {
    case 0x1906:
      a = b = -1;
      break;
    case 0x190A:
      a = b = -1;
      break;
    case 0x8049:
      a = b = -1;
      break;
    case 0x1907:
      a = 1;
      b = 2;
      break;
    default:
      return;
    }

  if (a >= 0)
    {
      unsigned char *d = z;
      unsigned int i;
      for (i = 0; i < n; i++)
	{
	  do
	    {
	      union
	      {
		float r;
		unsigned int i;
	      }
	      e;
	      e.r = c[i][1];
	      d[a] =
		((e.i >= 0x3f7f0000) ? ((int) e.i <
					    0) ? (unsigned char) 0
		 : (unsigned char) 255 : (e.r =
					  e.r * (255.0F / 256.0F) +
					  32768.0F, (unsigned char) e.i));
	    }
	  while (0);
	  d += y;
	}
    }

  if (b >= 0)
    {
      unsigned char *d = z;
      unsigned int i;
      for (i = 0; i < n; i++)
	{
	  do
	    {
	      union
	      {
		float r;
		unsigned int i;
	      }
	      e;
	      e.r = c[i][2];
	      d[b] =
		((e.i >= 0x3f7f0000) ? ((int) e.i <
					    0) ? (unsigned char) 0
		 : (unsigned char) 255 : (e.r =
					  e.r * (255.0F / 256.0F) +
					  32768.0F, (unsigned char) e.i));
	    }
	  while (0);
	  d += y;
	}
    }
}
