/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

union loc {  unsigned reg; signed offset; };
void __frame_state_for (volatile char *state_in, int x)
{
  /* We should move all the loads out of this loop. Right now, we only
     move one.  It takes two insertions because we insert a cast.  */
    union loc fs;
    int reg;
    for (;;)     {
        switch (x)  {
	    case 0:
		*state_in = fs.reg;
	    case 1:
		*state_in = fs.offset;
	}
    }
}

/* This is a weird testcase.  It should need PPRE to hoist the loop
   invariants and the volatileness of state_in prevents DSE of the
   first store.  Thus, this is XFAILed.  */

/* { dg-final { scan-tree-dump "Insertions: 2" "pre" { xfail *-*-* } } } */
