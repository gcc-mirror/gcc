// { dg-additional-options "-fmodules -fno-module-lazy" }

import hello;
#include <string_view>
int main (void)
{
  greeter ("world");
  return 0;
}
