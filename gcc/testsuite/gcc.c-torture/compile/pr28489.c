/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

typedef int c_int;
union c_insn
{
  void (*op) ();
  c_int *mem;
  c_int imm;
};
static union c_insn c_stack[((0x100 + 4) * 4)];
static struct c_ident *c_funcs;
static void (*c_op_bz) ();
static void c_direct (union c_insn *addr);
void
c_compile (int (*ext_getchar) (), void (*ext_rewind) (),
	   struct c_ident *externs)
{
  c_direct (((void *) 0));
}
static void
c_direct (union c_insn *addr)
{
  union c_insn *pc = addr;
  union c_insn *sp = c_stack;
  c_int imm = 0;
  static void *ops[] = {
    &&op_index, &&op_assign, &&op_add_a, &&op_sub_a, &&op_mul_a, &&op_div_a,
      &&op_mod_a, &&op_or_a, &&op_xor_a, &&op_and_a, &&op_shl_a, &&op_shr_a,
  };
    {
      c_op_bz = &&op_bz;
    }
  goto *(pc++)->op;
op_bz:if (imm)
    {
    }
op_push_imm_imm:(sp - 2)->imm = imm;
  goto *(pc - 1)->op;
op_index:imm = *((sp - 3)->mem += imm);
op_assign:*(sp - 3)->mem = imm;
op_add_a:imm = *(sp - 3)->mem += imm;
op_sub_a:imm = *(sp - 3)->mem -= imm;
op_mul_a:imm = *(sp - 3)->mem *= imm;
op_div_a:imm = *(sp - 3)->mem /= imm;
op_mod_a:imm = *(sp - 3)->mem %= imm;
op_or_a:imm = *(sp - 3)->mem |= imm;
op_xor_a:imm = *(sp - 3)->mem ^= imm;
op_and_a:imm = *(sp - 3)->mem &= imm;
op_shl_a:imm = *(sp - 3)->mem <<= imm;
op_shr_a:imm = *(sp - 3)->mem >>= imm;
}
