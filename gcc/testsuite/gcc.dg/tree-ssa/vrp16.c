/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp-details" } */


extern void abort (void) __attribute__ ((__noreturn__));
struct rtx_def;
typedef struct rtx_def *rtx;
struct rtx_def
{
  int code;
};
int
nonlocal_mentioned_p (rtx x)
{
  if (x->code == 6 || x->code == 7)
    if (x->code == 7)
      if (x->code != 7)
	abort ();
}

/* { dg-final { scan-tree-dump-times "Folding predicate .*to 0" 1 "vrp" } } */
/* { dg-final { cleanup-tree-dump "vrp" } } */

