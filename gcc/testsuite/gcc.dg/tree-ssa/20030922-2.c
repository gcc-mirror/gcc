/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom1" } */

struct rtx_def;
typedef struct rtx_def *rtx;
struct rtx_def
{
  int bb;
};
static int *block_to_bb;
static int target_bb;
static int
rgn_rank (rtx insn1, rtx insn2)
{
  if (block_to_bb[insn1->bb] != block_to_bb[insn2->bb])
    if (block_to_bb[insn2->bb] == target_bb
	&& block_to_bb[insn1->bb] != target_bb)
      return 1;
}

/* There should be two IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 2 "dom1" } } */
