// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Andrey Slepuhin <pooh@msu.ru>

template <const int&> struct X {};

int a = 1;
X<a> x; // ok, a has external linkage

const int b = 2;
X<b> y; // ERROR - const b has internal linkage

extern const int c;
X<c> z; // ok, c has external linkage

extern const int c = 3;
X<c> z_; // gets bogus error - using c as constant
