/* { dg-options "-O2" } */

#pragma GCC target "+nosimd+fp"

void
foo (char *char_ptr)
{
  __Float64x2_t *ptr = (__Float64x2_t *)(char_ptr + 1);
  asm volatile ("" :
		"=w" (ptr[1]),
		"=w" (ptr[2]),
		"=w" (ptr[3]),
		"=w" (ptr[4]));
}

/* { dg-final { scan-assembler-times {\tstp\tq[0-9]+, q[0-9]+} 2 } } */
