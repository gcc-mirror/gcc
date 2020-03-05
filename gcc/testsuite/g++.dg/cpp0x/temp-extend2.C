// PR c++/92831
// { dg-do run { target c++11 } }

template<typename T> using id = T;
struct S { S () { s++; } ~S () { s--; } S (int) { s++; } static int s; };
int S::s = 0;

void
bar (bool cond, bool cond2)
{
  if (S::s != (cond ? cond2 ? 7 : 5 : cond2 ? 8 : 9))
    __builtin_abort ();
}

void
foo (bool cond, bool cond2)
{
  int i = 1;
  // temporary array has same lifetime as a
  S&& a = id<S[3]>{1, 2, 3}[i];
  // temporary S has same lifetime as b
  const S& b = static_cast<const S&>(0);
  // exactly one of the four temporaries is lifetime-extended
  S&& c = cond ? cond2 ? id<S[3]>{1, 2, 3}[i] : static_cast<S&&>(0)
	       : cond2 ? id<S[4]>{1, 2, 3, 4}[i] : id<S[5]>{1, 2, 3, 4, 5}[i];
  bar (cond, cond2);
}

int
main ()
{
  foo (true, true);
  foo (true, false);
  foo (false, true);
  foo (false, false);
}
