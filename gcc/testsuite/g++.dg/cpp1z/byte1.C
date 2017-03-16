// Test for std::byte aliasing properties.
// { dg-options "-std=c++1z -O3" }

#include <cstddef>

using byte = std::byte;

enum class notbyte: unsigned char {} *np;

int main()
{
  int x;

  /* Stores through byte* can alias int, so the compiler can't optimize
     "x != 0".  */
  byte *p = (byte*)&x;
  x = 42;
  for (int i = 0; i < 4; ++i)
    p[i] = byte(0);
  if (x != 0)
    __builtin_abort();

  /* Stores through notbyte* mustn't alias int, so at -O3 the compiler should
     optimize "x != 42" to false.  */
  notbyte *np = (notbyte*)&x; 
  x = 42;
  for (int i = 0; i < 4; ++i)
    np[i] = notbyte(0);
  if (x != 42)
    __builtin_abort();
}
