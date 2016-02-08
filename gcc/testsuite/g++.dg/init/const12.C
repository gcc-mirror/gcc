// PR c++/69688
// { dg-do compile }
// { dg-options "-Wsign-compare" }

struct S
{
  static const int s;
  static const char c[];
  static wchar_t w[];

  S ()
    {
      for (int i = 0; i < s; i++)
	w[i] = 0;
    }
};

const char S::c[] = "x";
const int S::s = sizeof (S::c) - 1;
wchar_t S::w[S::s];
