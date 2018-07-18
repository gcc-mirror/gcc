// PR c++/78693
// { dg-do compile { target c++11 } }

template <class T>
void
foo (T t)
{
  auto i = t, j = 1;		// { dg-bogus "inconsistent deduction" }
}

template <class T>
void
bar (T t)
{
  auto i = 1, j = t, k = 2;	// { dg-bogus "inconsistent deduction" }
}

template <class T, class U>
void
foo (T t, U u)
{
  auto i = t, j = u;		// { dg-bogus "inconsistent deduction" }
}

void
foo ()
{
  foo (0);
  bar (0);
  foo (1, 2);
}
