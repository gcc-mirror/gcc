/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-linear -fdump-tree-ltrans-all" } */
typedef struct rtx_
{
} *rtx;
static rtx regno_save_mem[53][16 / 4 + 1];
extern set_mem_alias_set (rtx, rtx);
int main(void)
{
  int i, j;
  for (i = 0; i < 53; i++)
    for (j = (16 / (0 ? 8 : 4)); j > 0; j--)
      if (regno_save_mem[i][j] != 0)
        set_mem_alias_set (regno_save_mem[i][j], 0);
}

/* { dg-final { scan-tree-dump-times "Linear expression:  constant: 1   invariants:   denominator: 1" 1 "ltrans" } } */
/* { dg-final { scan-tree-dump-times "transformed loop" 1 "ltrans"} } */ 
