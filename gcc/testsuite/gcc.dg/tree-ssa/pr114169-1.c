/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop-details -fdump-tree-optimized" } */


/* PR tree-optimization/114169 */

#include <stdint.h>

struct S1 {
   uint32_t  f0;
   uint8_t  f1;
   uint64_t  f2;
   uint64_t  f3;
   int32_t  f4;
};

union U8 {
   struct S1  f0;
   int32_t  f1;
   int64_t  f2;
   uint8_t  f3;
   const int64_t  f4;
};

/* --- GLOBAL VARIABLES --- */
struct S1 g_16 = {4294967293UL,1UL,1UL,0xA9C1C73B017290B1LL,0x5ADF851FL};
union U8 g_37 = {{1UL,1UL,0x2361AE7D51263067LL,0xEEFD7F9B64A47447LL,0L}};
struct S1 g_50 = {0x0CFC2012L,1UL,0x43E1243B3BE7B8BBLL,0x03C5CEC10C1A6FE1LL,1L};


/* --- FORWARD DECLARATIONS --- */

void func_32(union U8 e) {
  e.f3 = e.f0.f4;
  g_16 = e.f0 = g_50;
}
/* The union e should not make a difference here.  */
/* { dg-final { scan-tree-dump-times "after previous" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump "g_16 = g_50;" "optimized" } } */
