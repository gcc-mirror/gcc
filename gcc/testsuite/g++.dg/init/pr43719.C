// PR c++/43719
// { dg-do compile }

struct A1		  // { dg-error "uninitialized" "" { target c++11 } }
{
  int const j; // { dg-message "should be initialized" "" { target c++98 } }
};

struct A2		  // { dg-error "uninitialized" "" { target c++11 } }
{
  int const volatile i; // { dg-message "should be initialized" "" { target c++98 } }
};

struct A3		  // { dg-error "uninitialized" "" { target c++11 } }
{
  int& ref; // { dg-message "should be initialized" "" { target c++98 } }
};

struct A4		  // { dg-error "uninitialized" "" { target c++11 } }
{
  int const& ref; // { dg-message "should be initialized" "" { target c++98 } }
};

struct A5		  // { dg-error "uninitialized" "" { target c++11 } }
{
  int& ref; // { dg-message "should be initialized" "" { target c++98 } }
  int const i; // { dg-message "should be initialized" "" { target c++98 } }
};

template <class T> struct S1 // { dg-error "uninitialized" "" { target c++11 } }
{
  T const i; // { dg-message "should be initialized" "" { target c++98 } }
};

template <class T> struct S2 // { dg-error "uninitialized" "" { target c++11 } }
{
  T const volatile i; // { dg-message "should be initialized" "" { target c++98 } }
};

template <class T> struct S3 // { dg-error "uninitialized" "" { target c++11 } }
{
  T& ref; // { dg-message "should be initialized" "" { target c++98 } }
};

template <class T> struct S4 // { dg-error "uninitialized" "" { target c++11 } }
{
  T const i; // { dg-message "should be initialized" "" { target c++98 } }
  T& ref; // { dg-message "should be initialized" "" { target c++98 } }
};

struct X
{
  X () : c (0), r (c) {}
  int const c;
  int const& r;
};

struct Y11		  // { dg-error "uninitialized" "" { target c++11 } }
{
  int const i; // { dg-message "should be initialized" "" { target c++98 } }
};

struct Y1		  // { dg-error "deleted" "" { target c++11 } }
{
  Y11 a[1];
};

struct Y22	       // { dg-error "uninitialized" "" { target c++11 } }
{
  int& ref; // { dg-message "should be initialized" "" { target c++98 } }
};

struct Y2		      // { dg-error "deleted" "" { target c++11 } }
{
  Y22 a[1];
};

struct Z1		// { dg-error "uninitialized" "" { target c++11 } }
{
  int const i; // { dg-message "should be initialized" "" { target c++98 } }
};

struct Z2		// { dg-error "uninitialized" "" { target c++11 } }
{
  int& ref; // { dg-message "should be initialized" "" { target c++98 } }
};

struct Z3		// { dg-error "uninitialized" "" { target c++11 } }
{
  int const i; // { dg-message "should be initialized" "" { target c++98 } }
};

struct Z4		// { dg-error "uninitialized" "" { target c++11 } }
{
  int& ref; // { dg-message "should be initialized" "" { target c++98 } }
};

struct Z5
{
  int i;
};

struct Z		// { dg-error "deleted" "" { target c++11 } }
{
  Z1 z1;
  Z2 z2;
  Z3 z3;
  Z4 z4;
  Z5 z5;
};

union U			// { dg-error "uninitialized" "" { target c++11 } }
{
  int const i; // { dg-message "should be initialized" "" { target c++98 } }
};


void f1 ()
{
  A1 a1; // { dg-error "uninitialized const member|deleted" }
}

void f2 ()
{
  A2 a2; // { dg-error "uninitialized const member|deleted" }
}

void f3 ()
{
  A3 a3; // { dg-error "uninitialized reference member|deleted" }
}

void f4 ()
{
  A4 a4; // { dg-error "uninitialized reference member|deleted" }
}

void f5 ()
{
  A5 a5; // { dg-error "uninitialized reference member|uninitialized const member|deleted" }
}

void f6 ()
{
  S1<int> s; // { dg-error "uninitialized const member|deleted" }
}

void f7 ()
{
  S2<int> s; // { dg-error "uninitialized const member|deleted" }
}

void f8 ()
{
  S3<int> s; // { dg-error "uninitialized reference member|deleted" }
}

void f9 ()
{
  S4<int> s; // { dg-error "uninitialized reference member|uninitialized const member|deleted" }
}

void f10 ()
{
  X x;
}

void f11 ()
{
  A1 a[ 1 ]; // { dg-error "uninitialized const member|deleted" }
}

void f12 ()
{
  A3 a[ 1 ]; // { dg-error "uninitialized reference member|deleted" }
}

void f13 ()
{
  Y1 y1; // { dg-error "uninitialized const member|deleted" }
}

void f14 ()
{
  Y2 y2; // { dg-error "uninitialized reference member|deleted" }
}

void f15 ()
{
  Z z; // { dg-error "uninitialized reference member|uninitialized const member|deleted" }
}

void f16 ()
{
  U u; // { dg-error "uninitialized const member|deleted" }
}

