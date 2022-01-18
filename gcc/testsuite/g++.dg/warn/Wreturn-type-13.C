// PR c++/103991
// { dg-do compile { target c++17 } }

struct S { ~S(); };
int
foo ()
{
  S s;
  if constexpr (true)
    return 0;
  else
    return 1;
}			// { dg-bogus "control reaches end of non-void function" }

#if __cpp_if_consteval >= 202106L
constexpr int
bar ()
{
  S s;
  if consteval
    {
      return 0;
    }
  else
    {
      return 1;
    }
}			// { dg-bogus "control reaches end of non-void function" }

int
baz ()
{
  return bar ();
}
#endif
