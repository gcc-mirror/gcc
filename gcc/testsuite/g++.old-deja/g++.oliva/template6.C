// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// simplified from bug report by Meenaradchagan Vishnu <mvishnu@fore.com>

template <typename> struct foo {};
template <> void foo(); // ERROR - bad specialization
