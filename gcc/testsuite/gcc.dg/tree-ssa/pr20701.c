/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

typedef struct {
  int code;
} *rtx;

int
can_combine_p (rtx insn, rtx elt)
{
  rtx set;

  set = 0;
  if (insn->code == 3)
    set = insn;
  else
    {
      set = elt;
      if (set == 0)
	return 0;
    }

  if (set == 0)
    return 1;

  return 0;
}

/* { dg-final { scan-tree-dump-times "Folding predicate.*to 0" 1 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
