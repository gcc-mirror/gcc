// PR c++/86981
// { dg-do compile { target c++11 } }
// { dg-options "-Wpessimizing-move" }

#include <string>
#include <tuple>
#include <utility>

std::tuple<std::string, std::string>
foo ()
{
  std::pair<std::string, std::string> p;
  return std::move (p);
}
