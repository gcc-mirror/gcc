// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Michael Matz 03 Mar 2002 <matz@suse.de>
// instance of an actual pattern in 252.eon from SPEC2000

// The last Wrapper<char> once wasn't completed when applying '='.

template <class T>
class Wrapper {
  public:
    Wrapper (T& a);
    Wrapper (const Wrapper<char>& ref);
};

template <class T>
class Element {
public:
  T * operator[](int x);
};

void test()
{
  char bla = 42;
  Element< Wrapper <unsigned char> > elem;
  elem[1][1] = Wrapper<char> (bla);
}
