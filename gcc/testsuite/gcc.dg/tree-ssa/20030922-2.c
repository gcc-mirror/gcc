/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2 -fdisable-tree-ifcombine" } */

struct rtx_def;
typedef struct rtx_def *rtx;
struct rtx_def
{
  int bb;
};
int *block_to_bb;
int target_bb;

int
rgn_rank (rtx insn1, rtx insn2)
{
  if (block_to_bb[insn1->bb] != block_to_bb[insn2->bb])
    if (block_to_bb[insn2->bb] == target_bb
	&& block_to_bb[insn1->bb] != target_bb)
      return 1;
}

/* There should be two IF conditionals.  */
/* We no longer record the conditional equivalence by design, thus we
   are unable to simplify the 3rd conditional at compile time.  */
/* { dg-final { scan-tree-dump-times "if " 2 "dom2" { xfail *-*-* } } } */
