// Negative examples from N3092 (FCD)
// { dg-do compile { target c++11 } }

// OK: declaration
constexpr int square(int x);	// { dg-message "never defined" }

// error: pixel is a type
constexpr struct pixel {        // { dg-error ".constexpr." }
  int x;
  int y;
  // OK: declaration
  constexpr pixel(int);
};
constexpr pixel::pixel(int a)
// OK: definition
  : x(square(a)), y(square(a))	// { dg-error "square" }
{ }

// error: square not defined, so small(2) not constant (5.19), so constexpr
// not satisfied
constexpr pixel small(2);	// { dg-message "in .constexpr. expansion of " }

// error: not for parameters
int next(constexpr int x) {	// { dg-error "parameter" }
  return x + 1;
}

// error: not a definition
extern constexpr int memsz;	// { dg-error "definition" }

// error: return type is void
constexpr void f(int x)		// { dg-error "void" "" { target c++11_only } }
{ /* ... */ }
// error: use of decrement
constexpr int prev(int x)
{ return --x; }			// { dg-error "-- x" "" { target c++11_only } }

// error: body not just return expr
constexpr int g(int x, int n) {
  int r = 1;
  while (--n > 0) r *= x;
  return r;
} // { dg-error "body of .constexpr. function" "" { target c++11_only } }

class debug_flag {
public:
  explicit debug_flag(bool);
  constexpr bool is_on(); // { dg-error "not a literal type" "" { target c++11_only } } debug_flag not literal type
private:
  bool flag;
};
// OK
constexpr int bar(int x, int y) // { dg-message "previously defined here" }
{ return x + y + x*y; }
// ...
// error: redefinition of bar
int bar(int x, int y)		// { dg-error "redefinition" }
{ return x * 2 + 3 * y; }

struct pixel2 {	   // { dg-message "no user-provided default constructor" }
  int x, y;
};
constexpr pixel2 ur = { 1294, 1024 };// OK
constexpr pixel2 origin;	     // { dg-error "uninitialized 'const" }

constexpr const int* addr(const int& ir) { return &ir; } // OK

// error, initializer for constexpr variable not a constant
extern constexpr const int* tp = addr(5); // { dg-error "" }
