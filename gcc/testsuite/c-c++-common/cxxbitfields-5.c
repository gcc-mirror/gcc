/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 --param allow-store-data-races=0" } */

#include <stdlib.h>

struct bits
{
  char a;
  int b:7;
  int c:9;
  unsigned char d;
} x;

struct bits *p;

static void allocit()
{
  p = (struct bits *) malloc (sizeof (struct bits));
}

/* Store into <c> should not clobber <d>.  */
/* We should not use a 32-bit move to store into p->, but a smaller move.  */
void foo()
{
  allocit();
  p -> c = 55;
}

/* { dg-final { scan-assembler "mov\[bw\]" } } */
