/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=hs" } */

int ashr1(int x) { return x >> 1; }
int ashr2(int x) { return x >> 2; }
int ashr3(int x) { return x >> 3; }
int ashr4(int x) { return x >> 4; }
int ashr5(int x) { return x >> 5; }
int ashr6(int x) { return x >> 6; }
int ashr7(int x) { return x >> 7; }
int ashr8(int x) { return x >> 8; }
int ashr9(int x) { return x >> 9; }
int ashr10(int x) { return x >> 10; }
int ashr11(int x) { return x >> 11; }
int ashr12(int x) { return x >> 12; }
int ashr13(int x) { return x >> 13; }
int ashr14(int x) { return x >> 14; }
int ashr15(int x) { return x >> 15; }
int ashr16(int x) { return x >> 16; }
int ashr17(int x) { return x >> 17; }
int ashr18(int x) { return x >> 18; }
int ashr19(int x) { return x >> 19; }
int ashr20(int x) { return x >> 20; }
int ashr21(int x) { return x >> 21; }
int ashr22(int x) { return x >> 22; }
int ashr23(int x) { return x >> 23; }
int ashr24(int x) { return x >> 24; }
int ashr25(int x) { return x >> 25; }
int ashr26(int x) { return x >> 26; }
int ashr27(int x) { return x >> 27; }
int ashr28(int x) { return x >> 28; }
int ashr29(int x) { return x >> 29; }
int ashr30(int x) { return x >> 30; }
int ashr31(int x) { return x >> 31; }

/* { dg-final { scan-assembler-times "asr_s\\s+r0,r0" 31 } } */
