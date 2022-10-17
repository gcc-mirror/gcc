// P2128R6
// { dg-do compile }
// { dg-options "-std=c++23" }

struct S
{
  S () : a {} {};
  int &operator[] () { return a[0]; }
  int &operator[] (int x) { return a[x]; }
  int &operator[] (int x, long y) { return a[x + y * 8]; }
  int a[64];
};

struct T
{
  operator int () { return 42; };
};

int buf[64];

struct U
{
  operator int * () { return buf; }
};

struct V
{
  V () : a {} {};
  V (int x, int y, int z) : a {x, y, z} {};
  int &operator[] () { return a[0]; }				// { dg-message "candidate" }
  int &operator[] (int x, long y) { return a[x + y * 8]; }	// { dg-message "candidate" }
  int a[64];
};

void
foo ()
{
  S s;
  T t;
  U u;
  V v;
  auto &a = buf[];		// { dg-error "built-in subscript operator without expression list" }
  auto &b = buf[1, 2];		// { dg-warning "top-level comma expression in array subscript changed meaning in" }
  auto &c = s[1, 2, 3];		// { dg-warning "top-level comma expression in array subscript changed meaning in" }
  auto &d = v[1];		// { dg-error "no match for 'operator\\\[\\\]' in 'v\\\[1\\\]' \\\(operand types are 'V' and 'int'\\\)" }
  auto &e = v[1, 2, 3];		// { dg-error "no match for call to 'V::operator\\\[\\\] \\\(int, int, int\\\)'" }
  auto &f = t[42, u];		// { dg-warning "top-level comma expression in array subscript changed meaning in" }
  auto &g = u[42, t];		// { dg-warning "top-level comma expression in array subscript changed meaning in" }
  auto &h = buf[42, 2.5];	// { dg-warning "top-level comma expression in array subscript changed meaning in" }
				// { dg-error "invalid types \[^\n\r]* for array subscript" "" { target *-*-* } .-1 }
}
