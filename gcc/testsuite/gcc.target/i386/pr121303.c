/* { dg-do compile } */
/* { dg-options "-O1 -favoid-store-forwarding" } */

typedef struct {
  bool is_ssa;
} nir_src;

nir_src nir_src_init;

typedef struct {
  nir_src src;
  char swizzle[6];
} nir_alu_src;

void nir_src_bit_size(nir_src);

void nir_lower_fb_read_instr() {
  {
    nir_alu_src alu_src = {nir_src_init}, src = alu_src;
    nir_src_bit_size(src.src);
  }
  {
    nir_alu_src alu_src = {nir_src_init}, src = alu_src;
    nir_src_bit_size(src.src);
  }
}
