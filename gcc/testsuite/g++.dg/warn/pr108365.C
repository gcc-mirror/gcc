// PR c++/108365
// { dg-do compile { target { { { ilp32 || lp64 } || llp64 } && c++11 } } }

constexpr char b = 1;
long t = (short) ((long long) (unsigned long long) (-__INT_MAX__ - 1) / (long long) (b ? -1 : 0)); // { dg-bogus "integer overflow in expression of type" }
