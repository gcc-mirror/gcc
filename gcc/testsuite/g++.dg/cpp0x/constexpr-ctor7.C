// PR c++/47199
// { dg-options "-std=c++11 -fno-elide-constructors" }

template < int > struct S
{
  constexpr S (int r):rr (r)
  {
  }
  S (const S &) = default;
  static constexpr S s ()
  {
    return -1;
  }
  int rr;
};

static const int d = S < 0 >::s ().rr;
