// { dg-additional-options -fmodules-ts }

#include <string_view>
import hello;
int main (void)
{
  greeter ("world");
  return 0;
}
