// PR c++/119150
// { dg-do run { target c++20 } }

consteval bool
foo (bool x)
{
  return x;
}

constexpr bool
bar ()
{
#if __cpp_if_consteval >= 202106L
  if consteval
    {
      return true;
    }
  else
    {
      return false;
    }
#else
  return __builtin_is_constant_evaluated ();
#endif
}

int
main ()
{
  bool a = false;
  a = foo (bar ());
  if (!a)
    __builtin_abort ();
  bool b = foo (bar ());
  if (!b)
    __builtin_abort ();
}
