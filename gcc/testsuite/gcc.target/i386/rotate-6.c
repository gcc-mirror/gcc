/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

/* scalar 64-bit DImode rotations.  */
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

/* DImode mem-to-mem rotations. These STV with -m32.  */
void mem1(unsigned long long *p) { *p = rot1(*p); }
void mem2(unsigned long long *p) { *p = rot2(*p); }
void mem3(unsigned long long *p) { *p = rot3(*p); }
void mem4(unsigned long long *p) { *p = rot4(*p); }
void mem5(unsigned long long *p) { *p = rot5(*p); }
void mem6(unsigned long long *p) { *p = rot6(*p); }
void mem7(unsigned long long *p) { *p = rot7(*p); }
void mem8(unsigned long long *p) { *p = rot8(*p); }
void mem9(unsigned long long *p) { *p = rot9(*p); }
void mem10(unsigned long long *p) { *p = rot10(*p); }
void mem15(unsigned long long *p) { *p = rot15(*p); }
void mem16(unsigned long long *p) { *p = rot16(*p); }
void mem17(unsigned long long *p) { *p = rot17(*p); }
void mem20(unsigned long long *p) { *p = rot20(*p); }
void mem24(unsigned long long *p) { *p = rot24(*p); }
void mem30(unsigned long long *p) { *p = rot30(*p); }
void mem31(unsigned long long *p) { *p = rot31(*p); }
void mem32(unsigned long long *p) { *p = rot32(*p); }
void mem33(unsigned long long *p) { *p = rot33(*p); }
void mem34(unsigned long long *p) { *p = rot34(*p); }
void mem40(unsigned long long *p) { *p = rot40(*p); }
void mem42(unsigned long long *p) { *p = rot42(*p); }
void mem48(unsigned long long *p) { *p = rot48(*p); }
void mem50(unsigned long long *p) { *p = rot50(*p); }
void mem56(unsigned long long *p) { *p = rot56(*p); }
void mem58(unsigned long long *p) { *p = rot58(*p); }
void mem60(unsigned long long *p) { *p = rot60(*p); }
void mem61(unsigned long long *p) { *p = rot61(*p); }
void mem62(unsigned long long *p) { *p = rot62(*p); }
void mem63(unsigned long long *p) { *p = rot63(*p); }

/* Check that rotN and memN give the same result.  */
typedef unsigned long long (*rotN)(unsigned long long);
typedef void (*memN)(unsigned long long*);

void eval(rotN s, memN v, unsigned long long x)
{
  unsigned long long r = s(x);
  unsigned long long t = x;
  v(&t);

  if (t != r)
    __builtin_abort ();
}

void test(rotN s, memN v)
{
  eval(s,v,0x0000000000000000ll);
  eval(s,v,0x0000000000000001ll);
  eval(s,v,0x0000000000000002ll);
  eval(s,v,0x0000000000000004ll);
  eval(s,v,0x0000000000000008ll);
  eval(s,v,0x0000000000000010ll);
  eval(s,v,0x0000000000000020ll);
  eval(s,v,0x0000000000000040ll);
  eval(s,v,0x0000000000000080ll);
  eval(s,v,0x0000000000000100ll);
  eval(s,v,0x0000000000000200ll);
  eval(s,v,0x0000000000000400ll);
  eval(s,v,0x0000000000000800ll);
  eval(s,v,0x0000000000001000ll);
  eval(s,v,0x0000000000002000ll);
  eval(s,v,0x0000000000004000ll);
  eval(s,v,0x0000000000008000ll);
  eval(s,v,0x0000000000010000ll);
  eval(s,v,0x0000000000020000ll);
  eval(s,v,0x0000000000040000ll);
  eval(s,v,0x0000000000080000ll);
  eval(s,v,0x0000000000100000ll);
  eval(s,v,0x0000000000200000ll);
  eval(s,v,0x0000000000400000ll);
  eval(s,v,0x0000000000800000ll);
  eval(s,v,0x0000000001000000ll);
  eval(s,v,0x0000000002000000ll);
  eval(s,v,0x0000000004000000ll);
  eval(s,v,0x0000000008000000ll);
  eval(s,v,0x0000000010000000ll);
  eval(s,v,0x0000000020000000ll);
  eval(s,v,0x0000000040000000ll);
  eval(s,v,0x0000000080000000ll);
  eval(s,v,0x0000000100000000ll);
  eval(s,v,0x0000000200000000ll);
  eval(s,v,0x0000000400000000ll);
  eval(s,v,0x0000000800000000ll);
  eval(s,v,0x0000001000000000ll);
  eval(s,v,0x0000002000000000ll);
  eval(s,v,0x0000004000000000ll);
  eval(s,v,0x0000008000000000ll);
  eval(s,v,0x0000010000000000ll);
  eval(s,v,0x0000020000000000ll);
  eval(s,v,0x0000040000000000ll);
  eval(s,v,0x0000080000000000ll);
  eval(s,v,0x0000100000000000ll);
  eval(s,v,0x0000200000000000ll);
  eval(s,v,0x0000400000000000ll);
  eval(s,v,0x0000800000000000ll);
  eval(s,v,0x0001000000000000ll);
  eval(s,v,0x0002000000000000ll);
  eval(s,v,0x0004000000000000ll);
  eval(s,v,0x0008000000000000ll);
  eval(s,v,0x0010000000000000ll);
  eval(s,v,0x0020000000000000ll);
  eval(s,v,0x0040000000000000ll);
  eval(s,v,0x0080000000000000ll);
  eval(s,v,0x0100000000000000ll);
  eval(s,v,0x0200000000000000ll);
  eval(s,v,0x0400000000000000ll);
  eval(s,v,0x0800000000000000ll);
  eval(s,v,0x1000000000000000ll);
  eval(s,v,0x2000000000000000ll);
  eval(s,v,0x4000000000000000ll);
  eval(s,v,0x8000000000000000ll);
  eval(s,v,0x0123456789abcdefll);
  eval(s,v,0x1111111111111111ll);
  eval(s,v,0x5555555555555555ll);
  eval(s,v,0x8888888888888888ll);
  eval(s,v,0xaaaaaaaaaaaaaaaall);
  eval(s,v,0xcafebabecafebabell);
  eval(s,v,0xdeadbeefdeadbeefll);
  eval(s,v,0xfedcba9876543210ll);
  eval(s,v,0xffffffffffffffffll);
}

int main()
{
  test(rot1,mem1);
  test(rot2,mem2);
  test(rot3,mem3);
  test(rot4,mem4);
  test(rot5,mem5);
  test(rot6,mem6);
  test(rot7,mem7);
  test(rot8,mem8);
  test(rot9,mem9);
  test(rot10,mem10);
  test(rot15,mem15);
  test(rot16,mem16);
  test(rot17,mem17);
  test(rot20,mem20);
  test(rot24,mem24);
  test(rot30,mem30);
  test(rot31,mem31);
  test(rot32,mem32);
  test(rot33,mem33);
  test(rot34,mem34);
  test(rot40,mem40);
  test(rot42,mem42);
  test(rot48,mem48);
  test(rot50,mem50);
  test(rot56,mem56);
  test(rot58,mem58);
  test(rot60,mem60);
  test(rot61,mem61);
  test(rot62,mem62);
  test(rot63,mem63);
  return 0;
}

