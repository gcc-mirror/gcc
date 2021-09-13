// { dg-module-do run }
// { dg-additional-options -fmodules-ts }

module;
#include <iostream>
#include <string_view>
export module hello;
// { dg-module-cmi hello }
export void greeter (std::string_view const &name)
{
  std::cout << "Hello " << name << "!\n";
}
