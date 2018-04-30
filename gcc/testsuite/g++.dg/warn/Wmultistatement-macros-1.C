// PR bootstrap/81216
// { dg-options "-Wmultistatement-macros" }
// { dg-do compile }

template <typename A, typename B>
static bool
equal (A a, A b, B c)
{
  for (; a != b; ++a, (void)++c)
    if (!(*a == *c))
      return false;
  return true;
}
