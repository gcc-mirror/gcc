/* { dg-do compile } */
/* { dg-options "-Os -fno-omit-frame-pointer" } */

struct _Unwind_Context { void *ra; };

long uw_install_context_1 (struct _Unwind_Context *, struct _Unwind_Context *);

void _Unwind_RaiseException(void)
{
  struct _Unwind_Context this_context, cur_context;
  long offset = uw_install_context_1 (&this_context, &cur_context);
  void *handler = __builtin_frob_return_addr ((&cur_context)->ra);

  __builtin_eh_return (offset, handler);
}

