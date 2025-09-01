/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -mrvv-max-lmul=m1 -fdump-tree-optimized" } */

typedef long long int64x8_t __attribute__((vector_size(64)));

int64x8_t foo(int64x8_t a, int64x8_t b)
{
    return a + b;
}
/* Make sure we can us up to LMUL 4 to process int64x8_t at once rather than
   break that into 4 LMUL 1 operations.  */
/* { dg-final { scan-assembler {vsetivli\s+zero,8,e64,m4,t[au],m[au]} } } */
