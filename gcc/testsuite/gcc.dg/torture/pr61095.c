/* { dg-do run } */
/* { dg-require-effective-target lp64 } */

extern void __attribute__ ((noreturn)) abort (void);

int __attribute__ ((noinline, noclone))
foo (unsigned long addr) {
    unsigned long *p = (unsigned long*)((addr & 0xffff83fffffffff8UL) * 4);
    unsigned long xxx = (unsigned long)(p + 1);
    return xxx >= 0x3c000000000UL;
}

int
main (void)
{
  if (foo (0))
    abort ();
  if (foo (0x7c0000000000UL))
    abort ();
  if (!foo (0xfc0000000000UL))
    abort ();
  return 0;
}
