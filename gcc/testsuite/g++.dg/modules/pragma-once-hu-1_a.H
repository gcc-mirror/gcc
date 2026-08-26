// PR c++/127097
// { dg-module-do run }
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

#pragma once

inline int
pragma_once_hu_f ()
{
  return 1;
}
