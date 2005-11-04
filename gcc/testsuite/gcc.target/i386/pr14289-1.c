/* PR middle-end/14289 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O0" } */

register int a[2] asm("ebx");

void Nase(void)
{
  int i=6;
  a[i]=5;  /* { dg-error "address of global" } */
}

