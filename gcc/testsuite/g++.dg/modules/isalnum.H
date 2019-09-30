// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

extern "C" 
{
  extern int isalnum (int) __attribute__ ((__nothrow__, __leaf__));
}

namespace std
{
using ::isalnum;
}
