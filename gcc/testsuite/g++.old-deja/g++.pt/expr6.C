// Build don't link:

// Based on a bug report by tveldhui <tveldhui@extreme.indiana.edu>

// excess errors test - XFAIL *-*-*

template <int X> class foo {};

foo< true ? 1 : 0 > bar;
