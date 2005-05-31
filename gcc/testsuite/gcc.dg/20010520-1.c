/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-w" } */

void f ()
{
  int i __asm__ ("%eax");
  __asm__ volatile ("" : "=a" (i));
}


