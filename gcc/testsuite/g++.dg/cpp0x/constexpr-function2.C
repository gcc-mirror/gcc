// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// From N2235

// Mess with the builtin by redeclaring.
constexpr int abs(int x) { return x < 0 ? -x : x; }

extern "C"
{
  constexpr float
  squaref(float x) { return x * x; }
}

// implicitly inline, already: warn?
inline constexpr double
squared(double x) { return x * x; }

constexpr int squarei(int x) { return x * x; }
extern const int side; // { dg-message "not initialized with a constant expression" }
constexpr int area = squarei(side); // { dg-error "side|argument" }
// error: squarei(side) is not a constant expression

int next(constexpr int x) // { dg-error "parameter" }
{ return x + 1; }

constexpr void f(int x)       // { dg-error "return type .void" }
{ /* ... */ }

constexpr int prev(int x)
{ return --x; }               // { dg-error "--" }

constexpr int g(int x, int n) // error: body not just ‘‘return expr’’
{
   int r = 1;
   while (--n > 0) r *= x;
   return r;
} // { dg-error "not a return-statement" }

constexpr int
bar(int x, int y) { return x + y + x * y; } // { dg-message "previously" }

int bar(int x, int y)	     // { dg-error "redefinition" }
{ return x * 2 + 3 * y; }

constexpr int twice(int x);  // { dg-message "never defined" }
enum { bufsz = twice(256) }; // { dg-error "" } twice() isn’t (yet) defined

constexpr int fac(int x)
{ return x > 2 ? x * fac(x - 1) : 1; } // OK
