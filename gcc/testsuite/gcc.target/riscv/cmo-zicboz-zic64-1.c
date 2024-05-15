/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zic64b_zicboz -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-allow-blank-lines-in-output 1 } */

/*
**clear_buf_123:
**    ...
**    cbo\.zero\t0\(a[0-9]+\)
**    sd\tzero,64\(a[0-9]+\)
**    sd\tzero,72\(a[0-9]+\)
**    sd\tzero,80\(a[0-9]+\)
**    sd\tzero,88\(a[0-9]+\)
**    sd\tzero,96\(a[0-9]+\)
**    sd\tzero,104\(a[0-9]+\)
**    sd\tzero,112\(a[0-9]+\)
**    sh\tzero,120\(a[0-9]+\)
**    sb\tzero,122\(a[0-9]+\)
**    ...
*/
int
clear_buf_123 (void *p)
{
  p = __builtin_assume_aligned(p, 64);
  __builtin_memset (p, 0, 123);
}

/*
**clear_buf_128:
**    ...
**    cbo\.zero\t0\(a[0-9]+\)
**    addi\ta[0-9]+,a[0-9]+,64
**    cbo\.zero\t0\(a[0-9]+\)
**    ...
*/
int
clear_buf_128 (void *p)
{
  p = __builtin_assume_aligned(p, 64);
  __builtin_memset (p, 0, 128);
}
