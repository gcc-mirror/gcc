// { dg-do compile }
// { dg-additional-options "-O2 -fdump-tree-optimized" }
#include <string>


__attribute__ ((pure))
extern int foo (const std::string &);

int
bar ()
{
  return foo ("abc") + foo (std::string("abc"));
}
// We used to add terminating zero explicitely instead of using fact
// that memcpy source is already 0 terminated.
