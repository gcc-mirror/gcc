// Positive examples from N3092 (FCD)
// { dg-do compile { target c++11 } }

#define SA(X) static_assert(X, #X)

constexpr int bufsz = 1024; // OK: definition
SA (bufsz == 1024);

constexpr int square(int x); // OK: declaration

struct pixel {
  int x;
  int y;
  // OK: declaration
  constexpr pixel(int);
};
constexpr pixel::pixel(int a) // OK: definition
  : x(square(a)), y(square(a))
{ }

constexpr int square(int x) // OK: definition
{ return x * x; }

constexpr pixel large(4); // OK: square defined
SA(large.x == 16 && large.y==16);

constexpr long long_max() // OK
{ return 2147483647; }

SA(long_max() == 2147483647);

constexpr int abs(int x) // OK
{ return x < 0 ? -x : x; }

SA(abs(-1) == 1);
SA(abs(24) == 24);

struct Length {
  explicit constexpr Length(int i = 0) : val(i) { }
private:
  int val;
};

constexpr Length l1;
constexpr Length l2(12);

struct pixel2 {
  int x, y;
};
constexpr pixel2 ur = { 1294, 1024 };// OK

SA(ur.x == 1294 && ur.y == 1024);

constexpr const int* addr(const int& ir) { return &ir; } // OK
static const int x = 5;
extern constexpr const int* xp = addr(x); // OK: (const int*)&(const int&)x
					  // is an address contant expression
SA(xp == &x);
extern constexpr int x2 = *addr(5);
SA(x2 == 5);
