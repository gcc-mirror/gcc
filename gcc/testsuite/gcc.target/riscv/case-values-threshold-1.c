/* { dg-do compile } */
/* { dg-options "-O2 -mtune=xt-c9501fdvt -ffat-lto-objects -fdump-tree-switchlower1" } */

#ifndef NUM_CASES
#define NUM_CASES 8
#endif

/* Give each case a distinct destination so that switch conversion
   cannot replace the dispatch with a table of return values.  */
extern int f0 (void), f1 (void), f2 (void), f3 (void);
extern int f4 (void), f5 (void), f6 (void), f7 (void);
extern int f8 (void), f9 (void), f10 (void);

int
dispatch (int x)
{
  switch (x)
    {
    case 0: return f0 ();
    case 1: return f1 ();
    case 2: return f2 ();
    case 3: return f3 ();
    case 4: return f4 ();
    case 5: return f5 ();
    case 6: return f6 ();
    case 7: return f7 ();
#if NUM_CASES >= 9
    case 8: return f8 ();
#endif
#if NUM_CASES >= 10
    case 9: return f9 ();
#endif
#if NUM_CASES >= 11
    case 10: return f10 ();
#endif
    default: return -1;
    }
}

/* { dg-final { scan-tree-dump "case clusters: 0 1 2 3 4 5 6 7" "switchlower1" } } */
