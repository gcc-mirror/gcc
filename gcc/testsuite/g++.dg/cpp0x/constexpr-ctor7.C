// PR c++/47199
// { dg-do compile { target c++11 } }
// { dg-options "-fno-elide-constructors" }

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
