// PR c++/105512
// { dg-additional-options -fmodules-ts }
// { dg-module-cmi Hello2 }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

module;
#include <string>
export module Hello2;

export std::string tester() {
  return "hello world\n";
}
