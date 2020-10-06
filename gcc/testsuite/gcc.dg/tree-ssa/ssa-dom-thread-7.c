/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-thread1-stats -fdump-tree-thread2-stats -fdump-tree-dom2-stats -fdump-tree-thread3-stats -fdump-tree-dom3-stats -fdump-tree-vrp2-stats -fno-guess-branch-probability" } */

/* Here we have the same issue as was commented in ssa-dom-thread-6.c.
   The PHI coming into the threader has a lot more constants, so the
   threader can thread more paths.

$ diff clean/a.c.105t.mergephi2 a.c.105t.mergephi2 
252c252
<   # s_50 = PHI <s_49(10), 5(14), s_51(18), s_51(22), 1(26), 1(29), 1(31), s_51(5), 4(12), 1(15), 5(17), 1(19), 3(21), 1(23), 6(25), 7(28), s_51(30)>
---
>   # s_50 = PHI <s_49(10), 5(14), 4(18), 5(22), 1(26), 1(29), 1(31), s_51(5), 4(12), 1(15), 5(17), 1(19), 3(21), 1(23), 6(25), 7(28), 7(30)>
272a273

  I spot checked a few and they all have the same pattern.  We are
  basically tracking the switch index better through multiple
  paths.  */

/* { dg-final { scan-tree-dump "Jumps threaded: 19"  "thread1" } } */
/* { dg-final { scan-tree-dump "Jumps threaded: 8" "thread2" } } */
/* { dg-final { scan-tree-dump-not "Jumps threaded"  "dom2" } } */

/* aarch64 has the highest CASE_VALUES_THRESHOLD in GCC.  It's high enough
   to change decisions in switch expansion which in turn can expose new
   jump threading opportunities.  Skip the later tests on aarch64.  */
/* { dg-final { scan-tree-dump-not "Jumps threaded"  "dom3" { target { ! aarch64*-*-* } } } } */
/* { dg-final { scan-tree-dump-not "Jumps threaded"  "vrp2" { target { ! aarch64*-*-* } } } } */

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
