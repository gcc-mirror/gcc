/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -fdump-tree-widening_mul-details" } */

/* The high half is also read by a second multiply, so the chain lowering
   cannot retire the product and only the sweep rewrites it.  */

typedef unsigned long long u64;
typedef unsigned int u32;

u64 x, y, cond, PL, PM, PM2, res, hi;

void
f (void)
{
  u64 PH;
  u64 CXH = x >> 32, CXL = (u32) x;
  u64 CYH = y >> 32, CYL = (u32) y;
  PM = CXH * CYL;
  PH = CXH * CYH;
  PL = CXL * CYL;
  PM2 = CXL * CYH;
  PH += PM >> 32;
  PM = (u32) PM + PM2 + (PL >> 32);
  hi = PH + (PM >> 32);
  if (cond)
    /* Keeps the high half, and so the product, live.  */
    res = (u32) hi * (PM >> 32);
}

/* { dg-final { scan-tree-dump "Narrowed high half of long multiply" "widening_mul" } } */
