/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom3" } */
 

typedef struct rs6000_stack {
  int first_gp_reg_save;
} rs6000_stack_t;
extern char regs_ever_live[113];
extern rs6000_stack_t *rs6000_stack_info (void);
void
rs6000_emit_prologue (int i, rs6000_stack_t *info)
{
  if (regs_ever_live[info->first_gp_reg_save + i] || i+info->first_gp_reg_save)
    gen_rtx_REG (info->first_gp_reg_save + i);
}

/* There should be precisely one load of first_gp_reg_save.  If there is
   more than one, then the dominator optimizations failed.  */
/* { dg-final { scan-tree-dump-times "first_gp_reg_save" 1 "dom3"} } */

/* There should be precisely one addition.  If there is more than one, then
   the dominator optimizations failed, most likely due to not handling
   commutative operands correctly.  */
/* { dg-final { scan-tree-dump-times "\\+" 1 "dom3"} } */
 

