// PR c++/100881
// { dg-additional-options "-std=c++20 -fmodules-ts" }
// { dg-module-cmi pr100881 }
module;
#include <source_location>
export module pr100881;

export
consteval int
current_line_fn(const std::source_location& loc = std::source_location::current())
{
  return loc.line();
}

export
struct current_line_cls
{
  int line = std::source_location::current().line();
};

export
template<class T>
consteval int
current_line_fn_tmpl(const std::source_location& loc = std::source_location::current())
{
  return loc.line();
}

export
template<class T>
struct current_line_cls_tmpl
{
  int line = std::source_location::current().line();
};
