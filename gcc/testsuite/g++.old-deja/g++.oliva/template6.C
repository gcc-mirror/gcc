// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// simplified from bug report by Meenaradchagan Vishnu <mvishnu@fore.com>

template <typename> struct foo {};
template <> void foo(); // { dg-error "not a template function" } bad specialization

struct baz {};
template <> void baz (); // { dg-error "not a template function" } bad specialization
