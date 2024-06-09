/* { dg-do compile } */
/* { dg-options "-O2" } */
void
foo (unsigned long long *a)
{
  register long long d asm ("r0") = 0x24;
  long long n;
  asm ("mr %0, %1" : "=r"(n) : "r"(d));
  *a++ = n;
}
