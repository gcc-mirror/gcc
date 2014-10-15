/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O3 -fselective-scheduling2 -funroll-loops" } */
extern int mode_size[];
typedef unsigned HARD_REG_SET[ ((64 + 32 - 1) / 32) ];
enum reload_type {
  RELOAD_FOR_INPUT,
  RELOAD_FOR_OUTPUT,
  RELOAD_FOR_INSN,
  RELOAD_FOR_INPUT_ADDRESS,
  RELOAD_FOR_OUTPUT_ADDRESS,
  RELOAD_FOR_OPERAND_ADDRESS,
  RELOAD_FOR_OPADDR_ADDR,
  RELOAD_OTHER,
  RELOAD_FOR_OTHER_ADDRESS
};
static HARD_REG_SET reload_reg_used;
static HARD_REG_SET reload_reg_used_in_input_addr[10];
static HARD_REG_SET reload_reg_used_in_output_addr[10];
static HARD_REG_SET reload_reg_used_in_input[10];
static HARD_REG_SET reload_reg_used_in_output[10];
static HARD_REG_SET reload_reg_used_in_op_addr;
static HARD_REG_SET reload_reg_used_in_op_addr_reload;
static HARD_REG_SET reload_reg_used_in_insn;
static HARD_REG_SET reload_reg_used_in_other_addr;
static HARD_REG_SET reload_reg_used_at_all;
void __attribute__((cold)) mark_reload_reg_in_use (int regno, int opnum, int type, int mode)
{
  int nregs = regno ? 1 : mode_size[mode];
  int i;
  for (i = regno; i < nregs + regno; i++)
  {
    switch (type)
    {
      case RELOAD_OTHER: reload_reg_used[i / 32u] |= 1 << i % 32u; break;
      case RELOAD_FOR_INPUT_ADDRESS: reload_reg_used_in_input_addr[opnum][i / 32u] |= 1 << i % 32u; break;
      case RELOAD_FOR_OUTPUT_ADDRESS: reload_reg_used_in_output_addr[opnum][i / 32u] |= 1 << i % 32u; break;
      case RELOAD_FOR_OPERAND_ADDRESS: reload_reg_used_in_op_addr[i / 32u] |= 1 << i % 32u; break;
      case RELOAD_FOR_OPADDR_ADDR: reload_reg_used_in_op_addr_reload[i / 32u] |= 1 << i % 32u; break;
      case RELOAD_FOR_OTHER_ADDRESS: reload_reg_used_in_other_addr[i / 32u] |= 1; break;
      case RELOAD_FOR_INPUT: reload_reg_used_in_input[opnum][i / 32u] |= 1 << i % 32u; break;
      case RELOAD_FOR_OUTPUT: reload_reg_used_in_output[opnum][i / 32u] |= 1 << i % 32u; break;
      case RELOAD_FOR_INSN: reload_reg_used_in_insn[i / 32u] |= 1 << i % 32u;
    }
    reload_reg_used_at_all[i / 32u] |= 1 << i;
  }
}
