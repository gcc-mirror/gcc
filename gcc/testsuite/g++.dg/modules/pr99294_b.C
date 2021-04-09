// PR 99294, ICE with -fno-module-lazy on class completeness
// { dg-additional-options {-fmodules-ts -fno-module-lazy} }

#include "pr99294.h"
import foo;

string Quux ()
{
  return 1;
}

// ICED complaining about Quux RETURN_DECL during gimple expand
