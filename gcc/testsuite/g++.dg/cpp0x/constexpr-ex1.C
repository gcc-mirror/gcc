// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// From N2235

// 4.1 constant-expression functions
// 1 examples





// 2 defined before first use
// NOTE: this is only needed in contexts that require a constant-expression
struct S {
    constexpr int twice();
    constexpr int t();		// { dg-message "used but never defined" }
private:
    static constexpr int val = 7;  // constexpr variable
};

constexpr int S::twice() { return val + val; }
constexpr S s = { };
int x1 = s.twice();     // ok
int x2 = s.t();         // error: S::t() not defined
constexpr int x2a = s.t();     // { dg-error "S::t" } error: S::t() not defined
constexpr int ff();     // ok
constexpr int gg();     // ok
int x3 = ff();          // error: ff() not defined
constexpr int x3a = ff();      // { dg-error "ff" } error: ff() not defined
constexpr int ff() { return 1; }        // too late
constexpr int gg() { return 2; }
int x4 = gg();  // ok


// 4.2 const-expression data

// 2
// storage  not allocated untill address taken
constexpr double x = 9484.748;
const double* p = &x;          // the &x forces x into memory

// 4.3 constant-expression constructors

// 1
struct complex {
   constexpr complex(double r, double i) : re(r), im(i) { }
   constexpr double real() { return re; }
   constexpr double imag() { return im; }
private:
   double re;
   double im;
};
constexpr complex I(0, 1);  // OK -- literal complex


// 2 invoked with non-const args
double x5 = 1.0;	       // { dg-message "not declared .constexpr" }
constexpr complex unit(x5, 0);	// { dg-error "x5|argument" } error: x5 non-constant
const complex one(x5, 0);   // OK, ‘‘ordinary const’’ -- dynamic
                           //   initialization
constexpr double xx = I.real(); // OK
complex z(2, 4);           // OK -- ordinary variable

// 3
constexpr complex v[] = {
     complex(0, 0), complex(1, 1), complex(2, 2)
};
constexpr double x6 = v[2].real(); // OK

// 4 
  constexpr int i = 98;
  typedef __INTPTR_TYPE__ intptr_t;
  constexpr intptr_t ip = (intptr_t) &i;	// { dg-error "constant" }

// 4.3.2 copy-constructor
constexpr complex operator+(complex z, complex w)
{
  return complex(z.real() + w.real(), z.imag() + w.imag()); // fine
}
constexpr complex I2 = I + I;                 // OK
struct resource {
  int id;
  constexpr resource(int i) : id(i) { }       // fine
  resource(const resource& r) : id(r.id)      // oops, not constexpr
  {
    //cout << id << " copied" << endl;
  }
};
constexpr resource f(resource d)
{ return d; }                  // { dg-error "non-constexpr" }
constexpr resource d = f(9);   // { dg-message "constexpr" }

// 4.4 floating-point constant expressions
