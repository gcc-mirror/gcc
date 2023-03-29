/* { dg-do compile } */
/* { dg-options "-O2 --param sched-autopref-queue-depth=1" } */

void f(int *a)
{
  for (;;)
    asm("" :: "r"(a[-0x10000000]), "r"(a[0x10000000]), "r"(a[0]) : "memory");
}
