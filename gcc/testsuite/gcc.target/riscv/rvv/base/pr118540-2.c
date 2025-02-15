/* { dg-do compile { target { rv32 } } } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -O3" { target { rv32 } } } */

long foo (long a, long b)
__attribute__((target("arch=rv64gcv_zbb")));

long foo (long a, long b)
{
  return a + (b * 2);
}

/* { dg-error "must start with rv32 but found 'rv64gcv_zbb'" "" { target { rv32 } } 0 } */
