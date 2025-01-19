// { dg-do run { target c++11 } }
// { dg-options "--embed-dir=${srcdir}/c-c++-common/cpp/embed-dir" }

const unsigned char m[] = {
  #embed <magna-carta.txt> limit (131)
};

template <int ...N>
int
foo ()
{
  unsigned char a[] = { N... };
  for (int i = 0; i < sizeof (a); ++i)
    if (a[i] != m[i])
      return -1;
  return sizeof (a);
}

template <typename ...T>
int
bar (T... args)
{
  int a[] = { args... };
  for (int i = 0; i < sizeof (a) / sizeof (a[0]); ++i)
    if (a[i] != m[i])
      return -1;
  return sizeof (a) / sizeof (a[0]);
}

int
main ()
{
  if (foo <
    #embed <magna-carta.txt> limit (1)
      > () != 1)
    __builtin_abort ();
  if (foo <
    #embed <magna-carta.txt> limit (6)
      > () != 6)
    __builtin_abort ();
  if (foo <
    #embed <magna-carta.txt> limit (131)
      > () != 131)
    __builtin_abort ();
  if (bar (
    #embed <magna-carta.txt> limit (1)
      ) != 1)
    __builtin_abort ();
  if (bar (
    #embed <magna-carta.txt> limit (6)
      ) != 6)
    __builtin_abort ();
  if (bar (
    #embed <magna-carta.txt> limit (131)
      ) != 131)
    __builtin_abort ();
}
