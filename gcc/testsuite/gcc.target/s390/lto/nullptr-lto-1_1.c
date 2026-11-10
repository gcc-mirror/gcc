/* Part of the LTO set, see nullptr-lto-1_0.c.  */

#include <stddef.h>

[[gnu::noinline]]
nullptr_t
lto_extern (void)
{
  __asm__ volatile ("");
  return nullptr;
}

[[gnu::noinline, gnu::weak]]
nullptr_t
lto_weak (void)
{
  __asm__ volatile ("");
  return nullptr;
}

[[gnu::noinline, gnu::visibility ("hidden")]]
nullptr_t
lto_hidden (void)
{
  __asm__ volatile ("");
  return nullptr;
}
