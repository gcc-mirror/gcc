/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc_zba_zbb_zbs" } */

unsigned long foo_sh1add_0x180011000 (void) { return 0x180011000UL; }
unsigned long foo_sh1add_0x180017000 (void) { return 0x180017000UL; }
unsigned long foo_sh1add_0x18001d000 (void) { return 0x18001d000UL; }
unsigned long foo_sh1add_0x180005000 (void) { return 0x180005000UL; }

unsigned long foo_sh2add_0x18000f000 (void) { return 0x18000f000UL; }
unsigned long foo_sh2add_0x180019000 (void) { return 0x180019000UL; }
unsigned long foo_sh2add_0x3c82b3000 (void) { return 0x3c82b3000UL; }

unsigned long foo_sh3add_0x380009000 (void) { return 0x380009000UL; }
unsigned long foo_sh3add_0x38001b000 (void) { return 0x38001b000UL; }
unsigned long foo_sh3add_0x5914a6000 (void) { return 0x5914a6000UL; }
unsigned long foo_sh3add_0x7cf04c000 (void) { return 0x7cf04c000UL; }

unsigned long foo_sh1add_0x1ffffd000 (void) { return 0x1ffffd000UL; }
unsigned long foo_sh2add_0x3ffffb000 (void) { return 0x3ffffb000UL; }
unsigned long foo_sh3add_0x7ffff7000 (void) { return 0x7ffff7000UL; }

unsigned long foo_neg_0x80000000 (void) { return 0x80000000UL; }
unsigned long foo_neg_0x180000000 (void) { return 0x180000000UL; }
unsigned long foo_neg_0x380000000 (void) { return 0x380000000UL; }

unsigned long foo_neg_0x400000000 (void) { return 0x400000000UL; }

unsigned long foo_neg_0x800000000 (void) { return 0x800000000UL; }
unsigned long foo_neg_0x1800050000000 (void) { return 0x1800050000000UL; }

unsigned long foo_neg_0x7ffffffff (void) { return 0x7ffffffffUL; }

unsigned long foo_neg_0x37fff7000 (void) { return 0x37fff7000UL; }

/* { dg-final { scan-assembler-times {\msh[123]add\.uw\M} 14 } } */
