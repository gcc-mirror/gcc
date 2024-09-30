// { dg-additional-options "-fmodules-ts" }
// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }
module;
#include <cstdio>
#include <string_view>
export module foo;
// { dg-module-cmi foo }
