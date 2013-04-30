// { dg-options "-pedantic" }
// { dg-do compile }

#include <limits>

// Compiling this with -pedantic was wrongly triggering this error:
// libstdc++-v3/include/limits:1269:45: warning : use of C++11 long long integer constant [-Wlong-long]
//       min() _GLIBCXX_USE_NOEXCEPT { return -__LONG_LONG_MAX__ - 1; }
//                                             ^
// libstdc++-v3/include/limits:1272:44: warning : use of C++11 long long integer constant [-Wlong-long]
//       max() _GLIBCXX_USE_NOEXCEPT { return __LONG_LONG_MAX__; }
//                                            ^
// libstdc++-v3/include/limits:1342:44: warning : use of C++11 long long integer constant [-Wlong-long]
//       max() _GLIBCXX_USE_NOEXCEPT { return __LONG_LONG_MAX__ * 2ULL + 1
//                                            ^

int
main ()
{
    return 0;
}
