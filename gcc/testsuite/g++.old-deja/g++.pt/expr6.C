// { dg-do assemble  }

// Based on a bug report by tveldhui <tveldhui@extreme.indiana.edu>

template <int X> class foo {};

foo< true ? 1 : 0 > bar;
