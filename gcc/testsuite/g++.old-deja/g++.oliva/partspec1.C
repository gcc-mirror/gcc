// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// simplified from bug report by Andrey Slepuhin <pooh@msu.ru>

template <typename A, int* P> struct X;

int a;

template <typename A>
struct X<A,&a> {};

int b;

template <typename A>
struct X<A,&b> {};

X<int,&a> x;
