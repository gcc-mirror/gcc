// PR c++/45923
// { dg-do compile { target c++11 } }

int f(int);

template <class T>
constexpr T g(T t) { return f(t); } // { dg-error "f.int" }

int main()
{
  constexpr int i = g(1);	// { dg-error "g.T" }
}

// --------------------

struct complex 			// { dg-message "no .constexpr. constructor" "" { target { ! implicit_constexpr } } }
{
  complex(double r, double i) : re(r), im(i) { }
  constexpr double real() const { return re; } // { dg-error "not a literal type|not usable in a constant expression" "" { target { ! implicit_constexpr } } }
  double imag() const { return im; }

private:
  double re;
  double im;
};

constexpr complex co1(0, 1);	   // { dg-error "19:the type .const complex. of .constexpr. variable .co1. is not literal" "" { target { ! implicit_constexpr } } }
constexpr double dd2 = co1.real(); // { dg-error "|in .constexpr. expansion of " "" { target { ! implicit_constexpr } } }

// --------------------

struct base		       // { dg-message "no .constexpr. constructor" "" { target { ! implicit_constexpr } } }
{
  int _M_i;
  base() : _M_i(5) { }
};

struct derived : public base	// { dg-message "base class" "" { target { ! implicit_constexpr } } }
{
  constexpr derived(): base() { } // { dg-error "non-.constexpr. function" "" { target { { ! implicit_constexpr } && c++20_down } } }
};

constexpr derived obj;		// { dg-error "not literal" "" { target { ! implicit_constexpr } } }

// --------------------

struct Def
{
  int _M_i;			// { dg-message "does not initialize" }

  constexpr Def() = default;	// { dg-error "implicit declaration is not .constexpr." "" { target c++17_down } }
};

constexpr Def defobj;		// { dg-error "uninitialized" }
