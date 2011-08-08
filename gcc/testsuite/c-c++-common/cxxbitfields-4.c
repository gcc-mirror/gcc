/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 --param allow-store-data-races=0" } */

struct bits
{
  char a;
  int b:7;
  int c:9;
  unsigned char d;
} x;

/* Store into <c> should not clobber <d>.  */
void update_c(struct bits *p, int val) 
{
    p -> c = val;
}

/* { dg-final { scan-assembler "mov\[bw\]" } } */
