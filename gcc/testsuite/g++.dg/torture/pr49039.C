// PR tree-optimization/49039
// { dg-do run }

template <class T1, class T2>
struct pair
{
  T1 first;
  T2 second;
  pair (const T1 & a, const T2 & b):first (a), second (b) {}
};

template <class T1, class T2>
inline pair <T1, T2>
make_pair (T1 x, T2 y)
{
  return pair <T1, T2> (x, y);
}

typedef __SIZE_TYPE__ size_t;
struct S
{
  const char *Data;
  size_t Length;
  static size_t min (size_t a, size_t b) { return a < b ? a : b; }
  static size_t max (size_t a, size_t b) { return a > b ? a : b; }
  S () :Data (0), Length (0) { }
  S (const char *Str) : Data (Str), Length (__builtin_strlen (Str)) {}
  S (const char *data, size_t length) : Data (data), Length (length) {}
  bool empty () const { return Length == 0; }
  size_t size () const { return Length; }
  S slice (size_t Start, size_t End) const
  {
    Start = min (Start, Length);
    End = min (max (Start, End), Length);
    return S (Data + Start, End - Start);
  }
  pair <S, S> split (char Separator) const
  {
    size_t Idx = find (Separator);
    if (Idx == ~size_t (0))
      return make_pair (*this, S ());
    return make_pair (slice (0, Idx), slice (Idx + 1, ~size_t (0)));
  }
  size_t find (char C, size_t From = 0) const
  {
    for (size_t i = min (From, Length), e = Length; i != e; ++i)
      if (Data[i] == C)
	return i;
    return ~size_t (0);
  }
};

void
Test (const char *arg)
{
  S Desc (arg);
  while (!Desc.empty ())
    {
      pair <S, S> Split = Desc.split ('-');
      S Token = Split.first;
      Desc = Split.second;
      if (Token.empty ())
	continue;
      Split = Token.split (':');
      S Specifier = Split.first;
      if (Specifier.empty ())
	__builtin_abort ();
    }
}

int
main ()
{
  Test ("-");
  return 0;
}
