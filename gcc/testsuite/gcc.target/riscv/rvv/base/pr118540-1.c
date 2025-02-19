/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" { target { rv64 } } } */

long foo (long a, long b)
__attribute__((target("arch=rv32gcv_zbb")));

long foo (long a, long b)
{
  return a + (b * 2);
}

/* { dg-error "must start with rv64 but found 'rv32gcv_zbb'" "" { target { rv64 } } 0 } */
