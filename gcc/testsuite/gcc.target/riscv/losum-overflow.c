/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32 -fno-section-anchors" } */

/* Check for %lo overflow.  Adding an offset larger than the alignment can
   overflow if the data is allocated to an address mod 4KB that is between
   2KB-offset+1 and 2KB-1.  */
typedef long long int int64_t;

#pragma pack(push)
#pragma pack(1)
struct S0 {
   signed f0 : 4;
   const volatile int64_t  f1;
   volatile signed f2 : 1;
   signed f3 : 31;
   unsigned f4 : 8;
   signed f5 : 20;
   unsigned f6 : 5;
};
#pragma pack(pop)

struct S0 g_3030 = {0,-9L,-0,-22553,7,-841,1};

int64_t
sub (void)
{
  return g_3030.f1;
}
/* { dg-final { scan-assembler-not "%lo\\(g_3030\\+4\\)" } } */
