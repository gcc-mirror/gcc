// PR c++/66639 - declare __func__ , __FUNCTION__ & __PRETTY_FUNCTION__
// as constexpr
// { dg-do compile { target c++11 } }

#define Assert(expr)   static_assert ((expr), #expr)
#define Compare(a, b)  Assert (0 == __builtin_strcmp (a, b))

constexpr const char* func ()
{
  return __func__;
}

constexpr const char* function ()
{
  return __FUNCTION__;
}

constexpr const char* pretty_function ()
{
  return __PRETTY_FUNCTION__;
}

constexpr const char* f0 = func ();
constexpr const char* f1 = function ();
constexpr const char* f2 = pretty_function ();

Compare (f0, "func");
Compare (f1, "function");
Compare (f2, "constexpr const char* pretty_function()");
