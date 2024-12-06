// { dg-do run { target c++23 } }
// { dg-options "--embed-dir=${srcdir}/c-c++-common/cpp/embed-dir" }

const unsigned char m[] = {
  #embed <magna-carta.txt> limit (136)
};

struct S
{
  S () : a {} {};
  template <typename ...T>
  int &operator[] (T... args)
  {
    int b[] = { args... };
    for (int i = 0; i < sizeof (b) / sizeof (b[0]); ++i)
      if (b[i] != m[i])
	return a[137];
    return a[sizeof (b) / sizeof (b[0])];
  }
  int a[138];
};

S s;

int
main ()
{
  if (&s[
      #embed <magna-carta.txt> limit (1)
	] != &s.a[1])
    __builtin_abort ();
  if (&s[
      #embed <magna-carta.txt> limit (6)
	] != &s.a[6])
    __builtin_abort ();
  if (&s[
      #embed <magna-carta.txt> limit (135)
	] != &s.a[135])
    __builtin_abort ();
}
