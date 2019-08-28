/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* "movdi" is about assigning 32-bit of data (byte).  *
 *                                                    *
 * This is one of the tests in series of testing for  *
 * "movdi" support. The test scenarios are as the     *
 * following table:                                   *
 *                                                    *
 * ,--------.----------.----------.-----------------. *
 * | test   | notation | dest.    | source          | *
 * |--------|---------------------------------------| *
 * | movd-1 | r <- i   | register | immediate       | *
 * | movd-2 | r <- r   |          | register        | *
 * | movd-3 | r <- m   |          | memory          | *
 * |--------+----------+----------+-----------------| *
 * | movd-4 | m <- r   |          | register        | *
 * | movd-5 | m <- i   | memroy   | immediate small | *
 * | movd-6 | m <- I   |          | immediate big   | *
 * | movd-7 | m <- m   |          | memory          | *
 * `--------^----------^----------^-----------------' */

/* assign immediate to register */
volatile int dummy;
void foo(void)
{
  volatile register int dst;
  dst = 0x12344321;
  dst = 0x40000000;
  dst = 0x7FFFFFFF;   /* largest positive number in 32-bit */
  dst = -2147483648;  /* smallest negative number in 32-bit */
  dst = 0xFFFFFFFF;  /* -1 */
  dummy = dst;
}
/* { dg-final { scan-assembler "mov_s\\s+r\[0-9\]+,305414945" } } */
/* { dg-final { scan-assembler "mov_s\\s+r\[0-9\]+,1073741824" } } */
/* { dg-final { scan-assembler "mov_s\\s+r\[0-9\]+,2147483647" } } */
/* { dg-final { scan-assembler "mov_s\\s+r\[0-9\]+,-2147483648" } } */
