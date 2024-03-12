/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=hs" } */

unsigned int shl1(unsigned int x) { return x << 1; }
unsigned int shl2(unsigned int x) { return x << 2; }
unsigned int shl3(unsigned int x) { return x << 3; }
unsigned int shl4(unsigned int x) { return x << 4; }
unsigned int shl5(unsigned int x) { return x << 5; }
unsigned int shl6(unsigned int x) { return x << 6; }
unsigned int shl7(unsigned int x) { return x << 7; }
unsigned int shl8(unsigned int x) { return x << 8; }
unsigned int shl9(unsigned int x) { return x << 9; }
unsigned int shl10(unsigned int x) { return x << 10; }
unsigned int shl11(unsigned int x) { return x << 11; }
unsigned int shl12(unsigned int x) { return x << 12; }
unsigned int shl13(unsigned int x) { return x << 13; }
unsigned int shl14(unsigned int x) { return x << 14; }
unsigned int shl15(unsigned int x) { return x << 15; }
unsigned int shl16(unsigned int x) { return x << 16; }
unsigned int shl17(unsigned int x) { return x << 17; }
unsigned int shl18(unsigned int x) { return x << 18; }
unsigned int shl19(unsigned int x) { return x << 19; }
unsigned int shl20(unsigned int x) { return x << 20; }
unsigned int shl21(unsigned int x) { return x << 21; }
unsigned int shl22(unsigned int x) { return x << 22; }
unsigned int shl23(unsigned int x) { return x << 23; }
unsigned int shl24(unsigned int x) { return x << 24; }
unsigned int shl25(unsigned int x) { return x << 25; }
unsigned int shl26(unsigned int x) { return x << 26; }
unsigned int shl27(unsigned int x) { return x << 27; }
unsigned int shl28(unsigned int x) { return x << 28; }
unsigned int shl29(unsigned int x) { return x << 29; }
unsigned int shl30(unsigned int x) { return x << 30; }
unsigned int shl31(unsigned int x) { return x << 31; }

/* { dg-final { scan-assembler-times "asl_s\\s+r0,r0,\[1-9\]" 31 } } */
