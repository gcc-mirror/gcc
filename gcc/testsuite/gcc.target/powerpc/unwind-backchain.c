/* -linux* targets have a fallback for the absence of unwind tables, thus are
   the only ones we can guarantee backtrace returns all addresses.  */
/* { dg-do run { target { *-*-linux* } } } */
/* { dg-options "-fno-asynchronous-unwind-tables" } */

#include <execinfo.h>

void
test_backtrace()
{
  int addresses;
  void *buffer[10];

  addresses = backtrace(buffer, 10);
  if(addresses != 4)
    __builtin_abort();
}

int
main()
{
  test_backtrace();
  return 0;
}
