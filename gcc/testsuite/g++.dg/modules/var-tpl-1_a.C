// { dg-module-do run }
// { dg-additional-options -fmodules-ts }

export module frob;
// { dg-module-cmi frob }

export template<typename T> T sum (T a)
{
  return a;
}

export template<typename T, typename... R>
inline T sum (T a, R... b)
{
  return a + static_cast<T> (sum (b...));
}

export inline int add (int a, int b, int c)
{
  return sum (a, b, c);
}
