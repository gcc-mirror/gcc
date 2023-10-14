/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=hs" } */

unsigned int lshr1(unsigned int x) { return x >> 1; }
unsigned int lshr2(unsigned int x) { return x >> 2; }
unsigned int lshr3(unsigned int x) { return x >> 3; }
unsigned int lshr4(unsigned int x) { return x >> 4; }
unsigned int lshr5(unsigned int x) { return x >> 5; }
unsigned int lshr6(unsigned int x) { return x >> 6; }
unsigned int lshr7(unsigned int x) { return x >> 7; }
unsigned int lshr8(unsigned int x) { return x >> 8; }
unsigned int lshr9(unsigned int x) { return x >> 9; }
unsigned int lshr10(unsigned int x) { return x >> 10; }
unsigned int lshr11(unsigned int x) { return x >> 11; }
unsigned int lshr12(unsigned int x) { return x >> 12; }
unsigned int lshr13(unsigned int x) { return x >> 13; }
unsigned int lshr14(unsigned int x) { return x >> 14; }
unsigned int lshr15(unsigned int x) { return x >> 15; }
unsigned int lshr16(unsigned int x) { return x >> 16; }
unsigned int lshr17(unsigned int x) { return x >> 17; }
unsigned int lshr18(unsigned int x) { return x >> 18; }
unsigned int lshr19(unsigned int x) { return x >> 19; }
unsigned int lshr20(unsigned int x) { return x >> 20; }
unsigned int lshr21(unsigned int x) { return x >> 21; }
unsigned int lshr22(unsigned int x) { return x >> 22; }
unsigned int lshr23(unsigned int x) { return x >> 23; }
unsigned int lshr24(unsigned int x) { return x >> 24; }
unsigned int lshr25(unsigned int x) { return x >> 25; }
unsigned int lshr26(unsigned int x) { return x >> 26; }
unsigned int lshr27(unsigned int x) { return x >> 27; }
unsigned int lshr28(unsigned int x) { return x >> 28; }
unsigned int lshr29(unsigned int x) { return x >> 29; }
unsigned int lshr30(unsigned int x) { return x >> 30; }
unsigned int lshr31(unsigned int x) { return x >> 31; }

/* { dg-final { scan-assembler-times "lsr_s\\s+r0,r0" 31 } } */
