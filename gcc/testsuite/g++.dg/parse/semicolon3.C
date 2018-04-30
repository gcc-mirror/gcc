// PR c++/45331
// { dg-do compile }
// { dg-options -std=c++98 }

struct OK1
{
  int a;
} // no complaints
  *s5;

struct OK2
{
  int a;
} // no complaints
  &s6 = *(new OK2());

struct OK3
{
  int a;
} // no complaints
  (s7);

struct OK5
{
  int a;
} ok5_var;			// no complaints

struct OK6
{
  int a;
} static ok6_var;		// no complaints

class OK7
{
public:
  OK7() { };
  int a;
} const ok7_var;		// no complaints

class OK8
{
  int a;
} extern ok8_var;		// no complaints

class OK9
{
  class OK9sub { int a; } mutable ok9sub; // no complaints
  int a;
};

int
autotest (void)
{
  struct OK10 { int a; } auto ok10 = { 0 }; // no complaints

  return ok10.a;
}

struct OK11
{
  int a;
} // no complaints
  const *ok11_var;

struct OK12
{
  int a;
} // no complaints
  const &ok12_var = *(new OK12());

struct OK13
{
  int a;
} // no complaints
  static *ok13_var;

class OK14
{
  struct OK14sub
  {
    int a;
  } // no complaints
    static &ok14_var;
};

class OK15
{
  int a;
} typedef tOK15;

class OK16
{
  int a;
} typedef *pOK16;

class OK17
{
  int a;
} typedef &rOK16;

struct E1
{
  int a;
} // { dg-error "after struct definition" }

typedef float BAR;

struct E2
{
  int a;
} // { dg-error "after struct definition" }

const int i0 = 1;

struct E3
{
  int a;
} // { dg-error "after struct definition" }

volatile long l0 = 1;

struct E4
{
  int a;
} // { dg-error "after struct definition" }

extern char c0;

struct E5
{
  int a;
} // { dg-error "after struct definition" }

static wchar_t wc0;

struct E6
{
  int a;
} // { dg-error "after struct definition" }

bool b0;

class E7
{
  int a;
} // { dg-error "after class definition" }

extern double d0;

class E8
{
  int a;
} // { dg-error "after class definition" }

inline short f(void)
{
  return 2;
}

class E9
{
  int a;
} // { dg-error "after class definition" }

class D0
{
  int a;
};

class E10
{
  int a;
} // { dg-error "after class definition" }

extern class D0 &f0 (void);

class E11
{
  int a;
} // { dg-error "after class definition" }

const struct E6 *f1 (void) { return 0; }

union U0 {
  int i;
  double d;
};

class E12
{
  int a;
} // { dg-error "after class definition" }

const union U0 *f2 (void) { return 0; }

enum e {
  U, V
};

class E13
{
  int a;
} // { dg-error "after class definition" }

static enum e f3 (void) { return U; }

union E14
{
  int i;
  double d;
} // { dg-error "after union definition" }

unsigned int i1 = 2;

union E15
{
  int i;
  double d;
} // { dg-error "after union definition" }

signed long l1 = 3;

class E16
{
  class sub0 { int a; }		// { dg-error "after class definition" }
    virtual int f2 (void);
} // { dg-error "after class definition" }

class E17
{
  class sub0 { int a; }		// { dg-error "after class definition" }
    mutable int i;
} // { dg-error "after class definition" }

class E18
{
  int a;
} // { dg-error "after class definition" }

typedef int E18int;

/* This was the original test from the PR.  */

class C0
{
public:
 int a;
} // { dg-error "after class definition" }

const int foo(const C0 &x)
{
 return x.a;
}
