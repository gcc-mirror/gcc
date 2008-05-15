/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

union loc {  unsigned reg; signed offset; };
void __frame_state_for (volatile char *state_in, int x)
{
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

/* { dg-final { scan-tree-dump-not "Insertions:" "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
