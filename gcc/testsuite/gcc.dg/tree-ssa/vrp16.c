/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-fre -fdump-tree-evrp" } */


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
  int code = x->code;
  if (code == 6 || code == 7)
    if (code == 7)
      if (code != 7)
	abort ();
}

/* { dg-final { scan-tree-dump-times "if" 0 "evrp" } } */

