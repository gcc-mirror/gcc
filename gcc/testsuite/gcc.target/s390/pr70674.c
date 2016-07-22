/* Test case for PR/70674.  */

/* { dg-do compile { target s390x-*-* } } */
/* { dg-options "-march=z10 -mtune=z196 -O2 -fno-omit-frame-pointer -fno-asynchronous-unwind-tables" } */

void
foo (void)
{
  volatile int a = 5;
  (void) a;
}

/* { dg-final { scan-assembler-not "^.*lgdr.*\n.*\\(%r11\\)" } } */
