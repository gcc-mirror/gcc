/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mavx512vl -mstv -mno-stackrealign" } */

unsigned long long rot1(unsigned long long x) { return (x>>1) | (x<<63); }
unsigned long long rot2(unsigned long long x) { return (x>>2) | (x<<62); }
unsigned long long rot3(unsigned long long x) { return (x>>3) | (x<<61); }
unsigned long long rot4(unsigned long long x) { return (x>>4) | (x<<60); }
unsigned long long rot5(unsigned long long x) { return (x>>5) | (x<<59); }
unsigned long long rot6(unsigned long long x) { return (x>>6) | (x<<58); }
unsigned long long rot7(unsigned long long x) { return (x>>7) | (x<<57); }
unsigned long long rot8(unsigned long long x) { return (x>>8) | (x<<56); }
unsigned long long rot9(unsigned long long x) { return (x>>9) | (x<<55); }
unsigned long long rot10(unsigned long long x) { return (x>>10) | (x<<54); }
unsigned long long rot15(unsigned long long x) { return (x>>15) | (x<<49); }
unsigned long long rot16(unsigned long long x) { return (x>>16) | (x<<48); }
unsigned long long rot17(unsigned long long x) { return (x>>17) | (x<<47); }
unsigned long long rot20(unsigned long long x) { return (x>>20) | (x<<44); }
unsigned long long rot24(unsigned long long x) { return (x>>24) | (x<<40); }
unsigned long long rot30(unsigned long long x) { return (x>>30) | (x<<34); }
unsigned long long rot31(unsigned long long x) { return (x>>31) | (x<<33); }
unsigned long long rot32(unsigned long long x) { return (x>>32) | (x<<32); }
unsigned long long rot33(unsigned long long x) { return (x>>33) | (x<<31); }
unsigned long long rot34(unsigned long long x) { return (x>>34) | (x<<30); }
unsigned long long rot40(unsigned long long x) { return (x>>40) | (x<<24); }
unsigned long long rot42(unsigned long long x) { return (x>>42) | (x<<22); }
unsigned long long rot48(unsigned long long x) { return (x>>48) | (x<<16); }
unsigned long long rot50(unsigned long long x) { return (x>>50) | (x<<14); }
unsigned long long rot56(unsigned long long x) { return (x>>56) | (x<<8); }
unsigned long long rot58(unsigned long long x) { return (x>>58) | (x<<6); }
unsigned long long rot60(unsigned long long x) { return (x>>60) | (x<<4); }
unsigned long long rot61(unsigned long long x) { return (x>>61) | (x<<3); }
unsigned long long rot62(unsigned long long x) { return (x>>62) | (x<<2); }
unsigned long long rot63(unsigned long long x) { return (x>>63) | (x<<1); }

/* { dg-final { scan-assembler-times "vpro\[lr\]q" 29 } } */
