/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__ ((noinline, noclone)) void
clobber_sp (void)
{
  __asm volatile ("" : : : "sp");
}

int
main (void)
{
  int ret;

  __asm volatile ("mov\tr4, #0xf4\n\t"
		  "mov\tr5, #0xf5\n\t"
		  "mov\tr6, #0xf6\n\t"
		  "mov\tr7, #0xf7\n\t"
		  "mov\tr0, #0xf8\n\t"
		  "mov\tr8, r0\n\t"
		  "mov\tr0, #0xfa\n\t"
		  "mov\tr10, r0"
		  : : : "r0", "r4", "r5", "r6", "r7", "r8", "r10");
  clobber_sp ();

  __asm volatile ("cmp\tr4, #0xf4\n\t"
		  "bne\tfail\n\t"
		  "cmp\tr5, #0xf5\n\t"
		  "bne\tfail\n\t"
		  "cmp\tr6, #0xf6\n\t"
		  "bne\tfail\n\t"
		  "cmp\tr7, #0xf7\n\t"
		  "bne\tfail\n\t"
		  "mov\tr0, r8\n\t"
		  "cmp\tr0, #0xf8\n\t"
		  "bne\tfail\n\t"
		  "mov\tr0, r10\n\t"
		  "cmp\tr0, #0xfa\n\t"
		  "bne\tfail\n\t"
		  "mov\t%0, #1\n"
		  "fail:\n\t"
		  "sub\tr0, #1"
		  : "=r" (ret) : :);
  return ret;
}
