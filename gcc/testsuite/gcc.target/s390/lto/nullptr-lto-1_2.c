/* { dg-options "-fno-lto -Wno-psabi" } */

/* Built without LTO, see nullptr-lto-1_0.c.  Disable psABI diagnostics because
   the test harness cannot treat them correctly.  */

#include <stddef.h>

[[gnu::noinline]]
nullptr_t
obj_extern (void)
{
  return nullptr;
}

[[gnu::noinline, gnu::weak]]
nullptr_t
obj_weak (void)
{
  return nullptr;
}

[[gnu::noinline, gnu::visibility ("hidden")]]
nullptr_t
obj_hidden (void)
{
  return nullptr;
}

nullptr_t (*volatile obj_fn_ptr) (void) = obj_extern;

/* Reference a function from the LTO set which has hidden visibility.  */

[[gnu::visibility ("hidden")]] extern nullptr_t hidden_used_by_obj (void);
nullptr_t (*volatile ref_hidden_used_by_obj) (void) = hidden_used_by_obj;
