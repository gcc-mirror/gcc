// C++ 26 P2795R5 - Erroneous behaviour for uninitialized reads
// { dg-do run { target c++26 } }
// { dg-skip-if "" { *-*-* } { "-ftrivial-auto-var-init=*" } { "" } }
// { dg-options "-O2 -Wuninitialized" }

#define assert(x) if (!(x)) __builtin_abort ()

template <typename T>
[[gnu::noipa]] T
baz (T &x)
{
  return x;
}

[[gnu::noipa]] int
foo (bool b)
{
  unsigned char c;
  unsigned char d = c;	// no erroneous behavior, but d has an erroneous value
			// { dg-warning "'c' is used uninitialized" "" { target *-*-* } .-1 }

  assert (c == d);	// holds, both integral promotions have erroneous behavior

  unsigned char f = c;
  unsigned char g = baz (f);

  assert (g == c);

  int e = d;		// erroneous behavior
  baz (e);
  return b ? d : 0;	// erroneous behavior if b is true
}

[[gnu::noipa]] void
bar ()
{
  int d1, d2;

  int e1 = d1;		// erroneous behavior
  int e2 = d1;		// erroneous behavior

  assert (e1 == e2);	// holds
  assert (e1 == d1);	// holds, erroneous behavior
  assert (e2 == d1);	// holds, erroneous behavior

  int f = d1;		// { dg-warning "'d1' is used uninitialized" }
  int g = baz (f);
  assert (g == d1);

  __builtin_memcpy (&d2, &d1, sizeof (int));	// no erroneous behavior, but d2 has an erroneous value
  assert (e1 == d2);	// holds, erroneous behavior
  assert (e2 == d2);	// holds, erroneous behavior
}

int
main ()
{
  foo (false);
  foo (true);
  bar ();
}
