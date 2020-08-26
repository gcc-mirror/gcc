// { dg-additional-options -fmodules-ts }

#include "tdef-inst-1.h"
import foo;

string Quux ()
{
  return 1; // failed to find converting ctor of string
}
