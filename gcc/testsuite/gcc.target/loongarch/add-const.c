/* { dg-do compile } */
/* { dg-options "-O -mabi=lp64d" } */

/* None of these functions should load the const operand into a temp
   register.  */

/* { dg-final { scan-assembler-not "add\\.[dw]" } } */

unsigned long f01 (unsigned long x) { return x + 1; }
unsigned long f02 (unsigned long x) { return x - 1; }
unsigned long f03 (unsigned long x) { return x + 2047; }
unsigned long f04 (unsigned long x) { return x + 4094; }
unsigned long f05 (unsigned long x) { return x - 2048; }
unsigned long f06 (unsigned long x) { return x - 4096; }
unsigned long f07 (unsigned long x) { return x + 0x7fff0000; }
unsigned long f08 (unsigned long x) { return x - 0x80000000l; }
unsigned long f09 (unsigned long x) { return x + 0x7fff0000l * 2; }
unsigned long f10 (unsigned long x) { return x - 0x80000000l * 2; }
unsigned long f11 (unsigned long x) { return x + 0x7fff0000 + 0x1; }
unsigned long f12 (unsigned long x) { return x + 0x7fff0000 - 0x1; }
unsigned long f13 (unsigned long x) { return x + 0x7fff0000 + 0x7ff; }
unsigned long f14 (unsigned long x) { return x + 0x7fff0000 - 0x800; }
unsigned long f15 (unsigned long x) { return x - 0x80000000l - 1; }
unsigned long f16 (unsigned long x) { return x - 0x80000000l + 1; }
unsigned long f17 (unsigned long x) { return x - 0x80000000l - 0x800; }
unsigned long f18 (unsigned long x) { return x - 0x80000000l + 0x7ff; }

unsigned int g01 (unsigned int x) { return x + 1; }
unsigned int g02 (unsigned int x) { return x - 1; }
unsigned int g03 (unsigned int x) { return x + 2047; }
unsigned int g04 (unsigned int x) { return x + 4094; }
unsigned int g05 (unsigned int x) { return x - 2048; }
unsigned int g06 (unsigned int x) { return x - 4096; }
unsigned int g07 (unsigned int x) { return x + 0x7fff0000; }
unsigned int g08 (unsigned int x) { return x - 0x80000000l; }
unsigned int g09 (unsigned int x) { return x + 0x7fff0000l * 2; }
unsigned int g10 (unsigned int x) { return x - 0x80000000l * 2; }
unsigned int g11 (unsigned int x) { return x + 0x7fff0000 + 0x1; }
unsigned int g12 (unsigned int x) { return x + 0x7fff0000 - 0x1; }
unsigned int g13 (unsigned int x) { return x + 0x7fff0000 + 0x7ff; }
unsigned int g14 (unsigned int x) { return x + 0x7fff0000 - 0x800; }
unsigned int g15 (unsigned int x) { return x - 0x80000000l - 1; }
unsigned int g16 (unsigned int x) { return x - 0x80000000l + 1; }
unsigned int g17 (unsigned int x) { return x - 0x80000000l - 0x800; }
unsigned int g18 (unsigned int x) { return x - 0x80000000l + 0x7ff; }
