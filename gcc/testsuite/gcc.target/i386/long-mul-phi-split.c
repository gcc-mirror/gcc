/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -fdump-tree-widening_mul-details" } */

/* The product has an operand PHI carrying a sign-extended value, which only
   long_mul_split_phi splits.  */

typedef unsigned long long u64;
typedef unsigned int u32;
typedef struct { u64 w[2]; } u128;

u128 pCS, ES;
u64 ARS1, ES32, R64H, S1, S3;
int src, flag;

void
f (void)
{
  ES.w[1] = src;			/* Signed, so this sign-extends.  */
  if (src < 0)
    {
      u64 PH;
      if (flag)
	ES.w[1]--;			/* Leaves the operand a PHI.  */
      u64 CXH = ES.w[1] >> 32, CXL = (u32) ES.w[1];
      u64 CYH = ARS1 >> 32, CYL = (u32) ARS1;
      u64 PM = CXH * CYL;
      PH = CXH * CYH;
      u64 PL = CXL * CYL, PM2 = CXL * CYH;
      PH += PM >> 32;
      PM = (u32) PM + PM2 + (PL >> 32);
      if (PH + (PM >> 32))
	R64H++;
      S3 = R64H;
    }
  {
    u64 PH;
    u64 CXH = ES32 >> 32, CXL = (u32) ES32;
    u64 CYH = ES.w[1] >> 32, CYL = (u32) ES.w[1];
    u64 PM = CXH * CYL;
    PH = CXH * CYH;
    u64 PL = CXL * CYL, PM2 = CXL * CYH;
    PH += PM >> 32;
    PM = (u32) PM + PM2 + (PL >> 32);
    if (PH + (PM >> 32))
      S1 = S3;
  }
  pCS.w[1] = S1;
}

/* { dg-final { scan-tree-dump "Split long-multiply operand PHI" "widening_mul" } } */
