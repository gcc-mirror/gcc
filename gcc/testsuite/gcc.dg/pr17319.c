/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dce" } */

typedef unsigned long long HARD_REG_ELT_TYPE;
typedef HARD_REG_ELT_TYPE HARD_REG_SET[2];
static HARD_REG_SET newpat_used_regs;
int try_combine (void)
{
 HARD_REG_ELT_TYPE *scan_tp_ = newpat_used_regs;
 scan_tp_[0] = 0;
 scan_tp_[1] = 0;
}
