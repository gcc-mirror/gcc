/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* "movqi" is about assigning 8-bit of data (byte).   *
 *                                                    *
 * This is one of the tests in series of testing for  *
 * "movqi" support. The test scenarios are as the     *
 * following table:                                   *
 *                                                    *
 * ,--------.----------.----------.-----------------. *
 * | test   | notation | dest.    | source          | *
 * |--------|---------------------------------------| *
 * | movq-1 | r <- i   | register | immediate       | *
 * | movq-2 | r <- r   |          | register        | *
 * | movq-3 | r <- m   |          | memory          | *
 * |--------+----------+----------+-----------------| *
 * | movq-4 | m <- r   |          | register        | *
 * | movq-5 | m <- i   | memroy   | immediate small | *
 * | movq-6 | m <- I   |          | immediate big   | *
 * | movq-7 | m <- m   |          | memory          | *
 * | movq-8 | m <- m   | volatile causes ext, why?  | *
 * `--------^----------^----------^-----------------' */

/* assign immediate to register */
volatile char dummy;
void foo(void)
{
  volatile register char dst;
  dst = 0x0;
  dst = 0x22;
  dst = 0x40;
  dst = 0x80;
  dst = -128;
  dst = 0xFF;
  dummy = dst;
}
/* { dg-final { scan-assembler "stb\\s+0,\\\[" } } */
/* { dg-final { scan-assembler "mov_s\\s+r\\d,34" } } */
/* { dg-final { scan-assembler "mov_s\\s+r\\d,64" } } */
/* expecting "mov_s r0,128" twice to cover both dst=-/+128 */
/* { dg-final { scan-assembler-times "mov\\s+r\\d,-128" 2 } } */
/* { dg-final { scan-assembler "stb\\s+-1,\\\[" } } */
