/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-march=z196 -O2" } */

extern void bar(int *mem);

void foo4(int *mem)
{
  int oldval = 0;
  if (!__atomic_compare_exchange_n (mem, (void *) &oldval, 1,
				    1, __ATOMIC_ACQUIRE, __ATOMIC_RELAXED))
    {
      bar (mem);
    }
}

/* { dg-final { scan-assembler {(?n)\n\tlt\t.*\n\tjne\t(\.L\d+)\n(.*\n)*\tcs\t.*\n\tber\t%r14\n\1:\n\tjg\tbar(@PLT)?\n} } } */
