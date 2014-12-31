// Contributed by Iain Sandoe <iain@codesourcery.com>, December 2014.  */
// { dg-do compile }
// { dg-options "-std=c++11 -Wno-deprecated" }

// Try to catch any problems in standard headers.

// n4296 Section 17.6.1.1 Table 14

#include <algorithm>
#include <array>
#include <atomic>
#include <bitset>
#include <chrono>
// NOT present at 5.0.0 #include <codecvt>
#include <complex>
#include <condition_variable>
#include <deque>
#include <exception>
#include <forward_list>
#include <fstream>
#include <functional>
// FIXME: CURRENTLY FAILS #include <future>
#include <initializer_list>
#include <iomanip>
#include <ios>
#include <iosfwd>
#include <iostream>
#include <istream>
#include <iterator>
#include <limits>
#include <list>
#include <regex>
#include <locale>
#include <scoped_allocator>
#include <map>
#include <set>
#include <memory>
#include <sstream>
#include <mutex>
#include <stack>
#include <new>
#include <stdexcept>
#include <numeric>
#include <streambuf>
#include <ostream>
#include <string>
#include <queue>
// LEGACY #include <strstream>
#include <system_error>
#include <ratio>
#include <thread>
#include <tuple>
#include <type_traits>
#include <typeindex>
#include <typeinfo>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <valarray>
#include <vector>

// n4296 Section 17.6.1.1 Table 15

#include <cassert>
#include <ccomplex>
#include <cctype>
#include <cerrno>
#include <cfenv>
#include <cfloat>
#include <cinttypes>
#include <ciso646>
#include <climits>
#include <clocale>
#include <cmath>
#include <csetjmp>
#include <csignal>
#include <cstdalign>
#include <cstdarg>
#include <cstdbool>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctgmath>
#include <ctime>
// NOT present everywhere #include <cuchar>
#include <cwchar>
#include <cwctype>
