/* { dg-do compile } */
/* { dg-options "-O2" } */

enum { TGSI_FILE_NULL };
struct ureg_src {
  unsigned File : 4;
  unsigned : 2;
  unsigned : 2;
  unsigned : 2;
  unsigned : 1;
  unsigned IndirectFile : 4;
  unsigned IndirectSwizzle : 2;
  int : 16;
  int : 6;
  int : 16;
  int : 16;
  unsigned : 10;
} __trans_tmp_1;

int ureg_src_indirect_addr_1, ntt_emit_texture_instr_sampler_handle_src;

void ureg_scalar(struct ureg_src);

void ntt_emit_texture_instr() {
  struct ureg_src sampler;
  if (ntt_emit_texture_instr_sampler_handle_src)
    sampler = __trans_tmp_1;
  struct ureg_src reg = sampler;
  reg.File != TGSI_FILE_NULL;
  reg.IndirectFile = reg.IndirectSwizzle = ureg_src_indirect_addr_1;
  sampler = reg;
  ureg_scalar(reg);
}
