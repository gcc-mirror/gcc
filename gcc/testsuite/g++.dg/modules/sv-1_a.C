// { dg-additional-options {-fmodules-ts -std=c++2a} }
module;
#include "sv-1.h"
export module Hello;
// { dg-module-cmi Hello }

export void SayHello (string_view const &name);
