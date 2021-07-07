#include <stdio.h>
#include <stdlib.h>

extern "C" void cpp_mytry (void)
{
  throw (int)9;
}

extern "C" void _M2_cpp_init (void) {}
extern "C" void _M2_cpp_finish (void) {}
