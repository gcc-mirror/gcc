// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// From N2235

constexpr int veryabs(int x) { return x < 0 ? -x : x; }

constexpr long long_max() { return 2147483647; }

constexpr int verysquare(int x) { return x * x; }
