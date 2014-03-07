// { dg-do compile { target c++11 } }

constexpr int verysquare(int x) { return x * x; }

const double mass = 9.8;
constexpr double energy = mass * verysquare(56.6); // { dg-error "mass" "" { xfail *-*-* } }

int arr[(int)mass];		// { dg-error "mass" "" { xfail *-*-* } }

float array[verysquare(9)];         // OK -- not C99 VLA

extern const int medium;
const int high = verysquare(medium); // OK -- dynamic initialization

enum { Max = verysquare(7) };      // OK
