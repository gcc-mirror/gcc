/* { dg-do compile } */
/* { dg-options "-O2 -std=c23 -Wpsabi" } */

/* Check that -Wpsabi diagnoses the changed ABI for returning nullptr_t and
   that the diagnostic is emitted only once per function.  Notes are emitted at
   call sites or for function definitions with external linkage or internal
   linkage where the address is taken.  */

#include <stddef.h>

[[gnu::noinline]]
nullptr_t
fun_extern_same_tu (void) /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
{
  __asm__ volatile ("");
  return nullptr;
}

[[gnu::noinline, gnu::weak]]
nullptr_t
fun_extern_same_tu_weak (void) /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
{
  __asm__ volatile ("");
  return nullptr;
}

static nullptr_t
fun_static_unused (void)
{
  __asm__ volatile ("");
  return nullptr;
}

[[gnu::noinline]]
static nullptr_t
fun_static (void)
{
  __asm__ volatile ("");
  return nullptr;
}

[[gnu::noinline]]
static nullptr_t
fun_static_addrtaken (void) /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
{
  __asm__ volatile ("");
  return nullptr;
}
nullptr_t (*fn_ptr) (void) = fun_static_addrtaken;

void
test_internal_call (void)
{
  fun_extern_same_tu ();
  fun_static ();
  fun_static_addrtaken ();
}

void
test_extern_same_tu_weak_call (void)
{
  fun_extern_same_tu_weak (); /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
}

void
test_indirect_call (void)
{
  /* Variable fn_ptr may have changed and we might no longer call
     fun_static_addrtaken.  */
  fn_ptr ();  /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
}

extern nullptr_t foo (void);
extern nullptr_t bar (void);

void
test_one_note_per_function (void)
{
  foo (); /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
  foo ();
  bar ();
}
