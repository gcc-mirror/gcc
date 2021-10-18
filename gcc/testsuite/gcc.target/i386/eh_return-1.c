/* { dg-do compile } */
/* { dg-options "-O2 -march=haswell -mno-avx512f -mtune-ctrl=avx256_move_by_pieces" } */

struct _Unwind_Context
{
  void *ra;
  char array[48];
};

extern long uw_install_context_1 (struct _Unwind_Context *);

void
_Unwind_RaiseException (void)
{
  struct _Unwind_Context this_context, cur_context;
  long offset = uw_install_context_1 (&this_context);
  __builtin_memcpy (&this_context, &cur_context,
		    sizeof (struct _Unwind_Context));
  void *handler = __builtin_frob_return_addr ((&cur_context)->ra);
  uw_install_context_1 (&cur_context);
  __builtin_eh_return (offset, handler);
}

/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%ymm" 4 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
