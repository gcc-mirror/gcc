/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-g -O -mx32 -maddress-mode=long" } */

void uw_init_context_1 (void *);
void _Unwind_ForcedUnwind (void)
{
  uw_init_context_1 (__builtin_dwarf_cfa ());
}
