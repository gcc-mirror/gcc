/* { dg-do compile } */
/* { dg-options "-O2 -fzero-call-used-regs=used" } */

void test_f0(void) {
#if __XCHAL_HAVE_FP
	asm volatile("":::"f0");
#endif
}

void test_b0(void) {
#if __XCHAL_HAVE_BOOLEANS
	asm volatile("":::"b0");
#endif
}

void test_acc(void) {
#if __XCHAL_HAVE_MAC16
	asm volatile("":::"acc");
#endif
}
