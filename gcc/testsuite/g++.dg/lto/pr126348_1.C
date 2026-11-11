#include "pr126348.h"

__attribute__ ((noinline)) int one (const Buffer &);
__attribute__ ((noinline)) int one_extra (const Buffer &);

void
fail (int, const char *, const Location &)
{
  __builtin_trap ();
}

void
note (const char *, int)
{
  asm volatile ("" ::: "memory");
}

__attribute__ ((noinline)) int
two (const Buffer &buffer)
{
  return buffer.printable_length ();
}

__attribute__ ((noinline)) int
two_extra (const Buffer &buffer)
{
  return buffer.printable_length ();
}
