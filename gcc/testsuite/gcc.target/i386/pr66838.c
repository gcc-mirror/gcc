/* { dg-do run { target lp64 } } */
/* { dg-options "-O2" } */

void abort (void);

char global;

__attribute__((sysv_abi, noinline, noclone))
void sysv_abi_func(char const *desc, void *local)
{
  register int esi asm ("esi");
  register int edi asm ("edi");
  
  if (local != &global)
    abort ();

  /* Clobber some of the extra SYSV ABI registers.  */
  asm volatile ("movl\t%2, %0\n\tmovl\t%2, %1"
		: "=r" (esi), "=r" (edi)
		: "i" (0xdeadbeef));
}

__attribute__((ms_abi, noinline, noclone))
void ms_abi_func ()
{
  sysv_abi_func ("1st call", &global);
  sysv_abi_func ("2nd call", &global);
  sysv_abi_func ("3rd call", &global);
}

int
main(void)
{
  ms_abi_func();
  return 0;
}
