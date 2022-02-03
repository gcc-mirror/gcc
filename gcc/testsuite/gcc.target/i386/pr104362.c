/* PR target/104362 */
/* { dg-do compile } */
/* { dg-options "-mavx512f" } */

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
  __builtin_memset(&this_context, 55, sizeof(this_context));
  long offset = uw_install_context_1 (&this_context);
  __builtin_memcpy (&this_context, &cur_context,
                    sizeof (struct _Unwind_Context));
  void *handler = __builtin_frob_return_addr ((&cur_context)->ra);
  uw_install_context_1 (&cur_context);
  __builtin_eh_return (offset, handler);
}
