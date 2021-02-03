// PR c++/98329
// { dg-do compile { target c++20 } }

template <typename To, typename From>
constexpr To
foo (const From &from)
{
  return __builtin_bit_cast (To, &from);
}

template <typename To, typename From>
constexpr To
bar (const From &from)
{
  return __builtin_bit_cast (To, *from);
}

template <typename To, typename From>
constexpr To
baz (const From &from)
{
  return __builtin_bit_cast (To, **from);
}

template <typename To, typename From>
constexpr To
qux (const From &from)
{
  return __builtin_bit_cast (To, -from);
}

void
test ()
{
  int i = 0;
  int *j = &i;
  int **k = &j;
  foo <char *> (i);
  bar <int> (j);
  baz <int> (k);
  qux <int> (i);
}
