/* { dg-do run } */
/* { dg-options "-O2" } */

/* Test that gcn_expand_scalar_to_vector_address does not clobber VCC.
   If it does then spills and reloads will be unsafe, leading to unexpected
   conditional branch behaviour.  */

extern void abort ();

__attribute__((vector_size(256))) int vec[2] = {{0}, {0}};

int
main()
{
  long vcc = 0;

  /* Load a known value into VCC.  The memory barrier ensures that the vector
     load must happen after this point.  */
  asm volatile ("s_mov_b32 vcc_lo, 0x12345689\n\t"
		"s_mov_b32 vcc_hi, 0xabcdef0"
		::: "memory");

  /* Compiler inserts vector load here.  */

  /* Consume the abitrary vector, and return the current value of VCC.  */
  asm volatile ("; no-op" : "=cV"(vcc) : "v"(vec[0]), "v"(vec[1]));

  /* The value should match the initialized value.  */
  if (vcc != 0xabcdef012345689)
    abort ();

  return 0;
}
