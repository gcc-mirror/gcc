// PR c++/48536
// { dg-options "-std=c++0x -pedantic-errors" }

#include <climits>

// According to C++11 / Clause 7.2/5 the following enumeration is
// well-formed.  It is also well-formed in C++03 if UINT_MAX < ULONG_MAX,
// but C++11 adds long long.

enum Enum_Inc  { EI_1=UINT_MAX, EI_2 }; // #1

// It is not equivalent to the following.
enum Enum_Inc2 { FI_1=UINT_MAX, FI_2=FI_1+1 }; // #2

#define SA(X) static_assert(X,#X)
SA (EI_2 != 0);
SA (FI_2 == 0);
