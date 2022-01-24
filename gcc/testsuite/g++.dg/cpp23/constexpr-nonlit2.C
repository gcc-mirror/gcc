// P2242R3
// { dg-do compile }
// { dg-options "-std=c++2b" }

constexpr int
foo ()
{
lab:
  return 1;
}

constexpr int
bar (int x)
{
  if (x)
    goto lab;		// { dg-error "'goto' is not a constant expression" }
  return 1;
lab:
  return 0;
}

constexpr int
baz (int x)
{
  if (!x)
    return 1;
  static int a;		// { dg-error "control passes through declaration of 'a' with static storage duration" }
  return ++a;
}

constexpr int
qux (int x)
{
  if (!x)
    return 1;
  thread_local int a;	// { dg-error "control passes through declaration of 'a' with thread storage duration" }
  return ++a;
}

struct S { S (); ~S (); int s; };	// { dg-message "'S::S\\\(\\\)' declared here" }

constexpr int
corge (int x)
{
  if (!x)
    return 1;
  S s;			// { dg-error "call to non-'constexpr' function 'S::S\\\(\\\)'" }
  return 0;
}

constexpr int a = bar (1);	// { dg-message "in 'constexpr' expansion of" }
constexpr int b = baz (1);	// { dg-message "in 'constexpr' expansion of" }
constexpr int c = qux (1);	// { dg-message "in 'constexpr' expansion of" }
constexpr int d = corge (1);	// { dg-message "in 'constexpr' expansion of" }
