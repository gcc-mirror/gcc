/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 --param allow-store-data-races=0" } */

/* Test that we don't store past VAR.K.  */

struct S
{
  volatile int i;
  volatile int j: 32;
  volatile int k: 15;
  volatile char c[2];
} var;

void setit()
{
  var.k = 13;
}

/* { dg-final { scan-assembler-not "movl.*, var" } } */
