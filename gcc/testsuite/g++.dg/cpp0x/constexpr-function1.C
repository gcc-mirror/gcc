// { dg-do compile { target c++11 } }

// From N2235

constexpr int veryabs(int x) { return x < 0 ? -x : x; }

constexpr long long_max() { return 2147483647; }

constexpr int verysquare(int x) { return x * x; }
