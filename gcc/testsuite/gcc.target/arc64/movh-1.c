/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* "movhi" is about assigning 16-bit of data (byte).  *
 *                                                    *
 * This is one of the tests in series of testing for  *
 * "movhi" support. The test scenarios are as the     *
 * following table:                                   *
 *                                                    *
 * ,--------.----------.----------.-----------------. *
 * | test   | notation | dest.    | source          | *
 * |--------|---------------------------------------| *
 * | movh-1 | r <- i   | register | immediate       | *
 * | movh-2 | r <- r   |          | register        | *
 * | movh-3 | r <- m   |          | memory          | *
 * |--------+----------+----------+-----------------| *
 * | movh-4 | m <- r   |          | register        | *
 * | movh-5 | m <- i   | memroy   | immediate small | *
 * | movh-6 | m <- I   |          | immediate big   | *
 * | movh-7 | m <- m   |          | memory          | *
 * | movh-8 | m <- m   | volatile causes sex, why?  | *
 * `--------^----------^----------^-----------------' */

/* assign immediate to register */
volatile short dummy;
void foo(void)
{
  volatile register short dst;
  dst = 0x1234;
  dst = 0x4000;
  dst = 0x7FFF;  /* largest positive number in short */
  dst = -32768;  /* smallest negative number in short */
  dst = 0xFFFF;  /* -1 */
  dummy = dst;
}
/* { dg-final { scan-assembler "mov_s\\s+r\[0-9\]+,4660" } } */
/* { dg-final { scan-assembler "mov_s\\s+r\[0-9\]+,16384" } } */
/* { dg-final { scan-assembler "mov_s\\s+r\[0-9\]+,32767" } } */
/* { dg-final { scan-assembler "mov_s\\s+r\[0-9\]+,-32768" } } */
/* { dg-final { scan-assembler "sth\.as\\s+-1,\\\[" } } */
