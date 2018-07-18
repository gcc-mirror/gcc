/* { dg-do link } */
/* { dg-options "-finstrument-functions" } */

/* Add empty implementations of __cyg_profile_func_enter() and
   __cyg_profile_func_exit() to avoid problems on non-glibc
   systems.  */
void __attribute__((no_instrument_function))
__cyg_profile_func_enter(void *this_fn, void *call_site)
{
}

void __attribute__((no_instrument_function))
__cyg_profile_func_exit(void *this_fn, void *call_site)
{
}

extern inline __attribute__((gnu_inline, always_inline)) int foo () { }
int main()
{
  foo ();
  return 0;
}
