/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom2-stats -fdump-tree-thread2-stats -fdump-tree-dom3-stats -fno-guess-branch-probability" } */

/* { dg-final { scan-tree-dump-not "Jumps threaded"  "dom2" } } */

/* We were previously checking for no threads in vrp-thread2, but now
   that we have merged the post and pre threaders, we get a dozen
   threads before VRP2.  */

/* aarch64 has the highest CASE_VALUES_THRESHOLD in GCC.  It's high enough
   to change decisions in switch expansion which in turn can expose new
   jump threading opportunities.  Skip the later tests on aarch64.  */
/* { dg-final { scan-tree-dump-not "Jumps threaded"  "dom3" { target { ! aarch64*-*-* } } } } */
/* { dg-final { scan-tree-dump "Jumps threaded: 9"  "thread2" { target { ! aarch64*-*-* } } } } */
/* { dg-final { scan-tree-dump "Jumps threaded: 17"  "thread2" { target { aarch64*-*-* } } } } */

enum STATE {
  S0=0,
  SI,
  S1,
  S2,
  S3,
  S4,
  S5,
  S6
};

int bar (enum STATE s);

enum STATE foo (unsigned char **y, unsigned *c)
{
  unsigned char *x = *y;
  unsigned char n;
  enum STATE s = S0;

  for( ; *x && s != SI; x++ )
    {
      n = *x;
      if (n == 'x')
	{
	  x++;
	  break;
	}
      switch(s)
	{
	case S0:
	  if(bar(n))
	    s = S3;
	  else if( n == 'a' || n == 'b' )
	    s = S1;
	  else if( n == 'c' )
	    s = S4;
	  else
	    {
	      s = SI;
	      c[SI]++;
	    }
	  c[S0]++;
	  break;
	case S1:
	  if(bar(n))
	    {
	      s = S3;
	      c[S1]++;
	    }
	  else if( n == 'c' )
	    {
	      s = S4;
	      c[S1]++;
	    }
	  else
	    {
	      s = SI;
	      c[S1]++;
	    }
	  break;
	case S3:
	  if( n == 'c' )
	    {
	      s = S4;
	      c[S3]++;
	    }
	  else if(!bar(n))
	    {
	      s = SI;
	      c[S3]++;
	    }
	  break;
	case S4:
	  if( n == 'E' || n == 'e' )
	    {
	      s = S2;
	      c[S4]++;
	    }
	  else if(!bar(n))
	    {
	      s = SI;
	      c[S4]++;
	    }
	  break;
	case S2:
	  if( n == 'a' || n == 'b' )
	    {
	      s = S5;
	      c[S2]++;
	    }
	  else
	    {
	      s = SI;
	      c[S2]++;
	    }
	  break;
	case S5:
	  if(bar(n))
	    {
	      s = S6;
	      c[S5]++;
	    }
	  else
	    {
	      s = SI;
	      c[S5]++;
	    }
	  break;
	case S6:
	  if(!bar(n))
	    {
	      s = SI;
	      c[SI]++;
	    }
	  break;
	default:
	  break;
	}
    }
  *y=x;
  return s;
}
