// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// simplified from bug report by Andrey Slepuhin <pooh@msu.ru>

template <typename A, int* P> struct X;

int a;

template <typename A>
struct X<A,&a> {}; // gets bogus error - candidate - XFAIL *-*-*

int b;

template <typename A>
struct X<A,&b> {}; // gets bogus error - candidate - XFAIL *-*-*

X<int,&a> x; // gets bogus error - ambiguous - XFAIL *-*-*
