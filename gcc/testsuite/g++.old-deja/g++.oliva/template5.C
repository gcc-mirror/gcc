// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// simplified from bug report by Andrey Slepuhin <pooh@msu.ru>

template <typename> class X {
  template <typename> class Z;
};

X<void> a;
