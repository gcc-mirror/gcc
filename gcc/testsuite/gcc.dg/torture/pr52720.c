/* { dg-do compile } */
/* { dg-options "-march=k8-sse3" { target i?86-*-* x86_64-*-* } } */

struct alu_bank_swizzle {
    int hw_gpr[3][4];
    int hw_cfile_addr[4];
};
void check_vector (struct alu_bank_swizzle *);
static void init_bank_swizzle(struct alu_bank_swizzle *bs)
{
  int i, cycle, component;
  for (cycle = 0; cycle < 3; cycle++)
    for (component = 0; component < 4; component++)
      bs->hw_gpr[cycle][component] = -1;
  for (i = 0; i < 4; i++) 
    bs->hw_cfile_addr[i] = -1;
}
int check_and_set_bank_swizzle(int max_slots, int *slots)
{
  struct alu_bank_swizzle bs;
  int i;
  for (i = 0; i < max_slots; i++)
    {
      init_bank_swizzle(&bs);
      if (slots[i])
	check_vector(&bs); 
    }
}

