/* { dg-do compile { target ia64-*-* } } */
/* { dg-options "-O2 -fpic" } */
/* { dg-final { scan-assembler-not "@ltoffx" } } */

/* Test imitates early ld.so setup in glibc
   where no dynamic relocations must be present. */

struct rtld_global
{
    long *p[77];
};

struct rtld_global _rtld_local __attribute__ ((visibility ("hidden"), section (".sdata")));

static void __attribute__ ((unused, noinline))
elf_get_dynamic_info (struct rtld_global * g, long * dyn)
{
  long **info = g->p;

  info[(0x6ffffeff - *dyn) + 66] = dyn;
}

void __attribute__ ((unused, noinline))
_dl_start (long * dyn)
{
  elf_get_dynamic_info(&_rtld_local, dyn);
}
