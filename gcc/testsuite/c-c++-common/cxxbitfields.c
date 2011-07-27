/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 --param allow-store-data-races=0" } */

/* Test that we don't store past VAR.A.  */

struct S
{
  volatile unsigned int a : 4;
  unsigned char b;
  unsigned int c : 6;
} var;

void set_a()
{
  var.a = 12;
}

/* { dg-final { scan-assembler-not "movl.*, var" } } */
