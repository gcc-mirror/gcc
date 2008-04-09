// { dg-do run  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Loren J. Rittle 07 Jun 2000 <ljrittle@acm.org>
//
// This test catches the occasional macro/symbol conflict between
// C++ and system-provided headers.

#include <algorithm>
#include <bitset>
#include <cassert>
#include <cctype>
#include <cerrno>
#include <cfloat>
#include <ciso646>
#include <climits>
#include <clocale>
#include <cmath>
#include <complex>
#include <csetjmp>
#include <csignal>
#include <cstdarg>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <deque>
#include <exception>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iosfwd>
#include <iostream>
#include <iterator>
#include <list>
#include <map>
#include <memory>
#include <new>
#include <numeric>
#include <queue>
#include <set>
#include <stack>
#include <stdexcept>
#include <string>
#include <typeinfo>
#include <utility>
// The VxWorks kernel-mode headers define macros named "max" and
// "min", which is not ISO-compliant, but is part of the VxWorks API.
#if defined __vxworks && !defined __RTP__
#undef max
#undef min
#endif
#include <valarray>
#include <vector>

int main ()
{
  return 0;
}
