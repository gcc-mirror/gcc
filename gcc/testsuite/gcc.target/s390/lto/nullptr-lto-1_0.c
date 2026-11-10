/* { dg-lto-do link } */
/* { dg-require-linker-plugin "" } */
/* { dg-lto-options { { -O2 -std=c23 -Wpsabi -flto -flto-partition=one -fuse-linker-plugin -fdump-tree-optimized } } } */

/* Check -Wpsabi diagnostics for returning nullptr_t when linking an executable
   with LTO and the linker plugin.  This file and nullptr-lto-1_1.c are part of
   the LTO set whereas nullptr-lto-1_2.c is built without LTO.  For the latter
   we have to assume that it was built with an older GCC and therefore expect
   diagnostics.

   Throughout the whole test
   __asm__ volatile ("");
   is used in order to prevent the removal of calls and whole functions.  We
   cannot rely on attribute noipa because this has effects on function
   visibility.  */

#include <stddef.h>

/* Defined in nullptr-lto-1_1.c, i.e., part of the LTO set.  */
extern nullptr_t lto_extern (void);
extern nullptr_t lto_weak (void);
[[gnu::visibility ("hidden")]] extern nullptr_t lto_hidden (void);

/* Defined in nullptr-lto-1_2.c, i.e., not part of the LTO set.  */
extern nullptr_t obj_extern (void);
extern nullptr_t obj_weak (void);
[[gnu::visibility ("hidden")]] extern nullptr_t obj_hidden (void);
extern nullptr_t (*volatile obj_fn_ptr) (void);

/* Check that no diagnostics are emitted for calls to functions defined in the
   LTO set.  The linker resolves them to the definition in the LTO set and
   nothing outside references them, so neither the calls nor the definitions
   are diagnosed.  */

[[gnu::noinline]]
void
call_lto_extern (void)
{
  lto_extern ();
}

[[gnu::noinline]]
void
call_lto_weak (void)
{
  lto_weak ();
}

[[gnu::noinline]]
void
call_lto_hidden (void)
{
  lto_hidden ();
}

/* Check that diagnostics are emitted for calls to functions which are defined
   in a non-LTO object.  */

[[gnu::noinline]]
void
call_obj_extern (void)
{
  obj_extern (); /* { dg-lto-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
}

[[gnu::noinline]]
void
call_obj_weak (void)
{
  obj_weak (); /* { dg-lto-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
}

[[gnu::noinline]]
void
call_obj_hidden (void)
{
  obj_hidden (); /* { dg-lto-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
}

[[gnu::noinline]]
void
call_indirect (void)
{
  obj_fn_ptr (); /* { dg-lto-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
}

/* Diagnose a hidden function which is referenced from a non-LTO object from
   nullptr-lto-1_2.c.  Here the linker plugin reports that the symbol is used
   from outside of the LTO set.  */

[[gnu::visibility ("hidden"), gnu::noinline]]
nullptr_t
hidden_used_by_obj (void) /* { dg-lto-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
{
  __asm__ volatile ("");
  return nullptr;
}

/* Do not diagnose a hidden function which is only referenced from the LTO set.  */

[[gnu::visibility ("hidden"), gnu::noinline]]
nullptr_t
hidden_unused_by_obj (void)
{
  __asm__ volatile ("");
  return nullptr;
}

int
main (void)
{
  call_lto_extern ();
  call_lto_weak ();
  call_lto_hidden ();
  call_obj_extern ();
  call_obj_weak ();
  call_obj_hidden ();
  call_indirect ();
  hidden_used_by_obj ();
  hidden_unused_by_obj ();
  return 0;
}

/* Make sure that all calls are still present before RTL expansion.  */
/* { dg-final { scan-ltrans-tree-dump "lto_extern\\S* \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "lto_weak\\S* \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "lto_hidden\\S* \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "obj_extern\\S* \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "obj_weak\\S* \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "obj_hidden\\S* \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "_\\d+ \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "  call_lto_extern \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "  call_lto_weak \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "  call_lto_hidden \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "  call_obj_extern \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "  call_obj_weak \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "  call_obj_hidden \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "  call_indirect \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "  hidden_used_by_obj \\(\\);" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump "  hidden_unused_by_obj \\(\\);" "optimized" } } */
