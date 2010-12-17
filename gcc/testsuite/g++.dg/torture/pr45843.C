// PR target/45843
// { dg-do run }

#include <stdarg.h>

extern "C" void abort ();
struct S { struct T { } a[14]; char b; };
struct S arg, s;

void
foo (int z, ...)
{
  char c;
  va_list ap;
  va_start (ap, z);
  c = 'a';
  arg = va_arg (ap, struct S);
  if (c != 'a')
    abort ();
  va_end (ap);
}

int
main ()
{
  foo (1, s);
  return 0;
}
