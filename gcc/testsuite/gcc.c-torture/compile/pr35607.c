/* { dg-require-effective-target indirect_calls } */

extern void (*__fini_array_start []) (void);
extern void (*__fini_array_end []) (void);
void
__libc_csu_fini (void)
{
  __SIZE_TYPE__ i = __fini_array_end - __fini_array_start;
  while (i-- > 0)
    (*__fini_array_start [i]) ();
}
