/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -O3 -mno-vsx" } */

/* target/60137, compiler got a 'could not split insn error'.  */

extern int target_flags;
extern char fixed_regs[53];
extern char call_used_regs[53];

void init_reg_sets_1(void)
{
  int i;
  for (i = 0; i < 53; i++)
    fixed_regs[i] = call_used_regs[i] = (call_used_regs[i] &((target_flags & 0x02000000) ? 2 : 1)) != 0;
}
