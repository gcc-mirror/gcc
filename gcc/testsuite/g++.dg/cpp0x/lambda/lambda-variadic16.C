// PR c++/100282
// { dg-do compile { target c++11 } }

template <typename... Ts>
void
local_class ()
{
  int { []{ struct ZZ : Ts {}; }... }; // { dg-bogus "" "" { xfail *-*-* } }
}

template // <>
void
local_class<int> ();
