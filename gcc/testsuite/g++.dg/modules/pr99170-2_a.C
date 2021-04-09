// pr99170 pending instantiations
// { dg-additional-options -fmodules-ts }
module;
#include "pr99170-2.h"

export  module  hello;
// { dg-module-cmi hello }

export void greeter (__gnu_cxx::char_traits<char> const &name);
