// PR c++/43719
// { dg-do compile }

struct A1
{
  int const j; // { dg-message "should be initialized" }
};

struct A2
{
  int const volatile i; // { dg-message "should be initialized" }
};

struct A3
{
  int& ref; // { dg-message "should be initialized" }
};

struct A4
{
  int const& ref; // { dg-message "should be initialized" }
};

struct A5
{
  int& ref; // { dg-message "should be initialized" }
  int const i; // { dg-message "should be initialized" }
};

template <class T> struct S1
{
  T const i; // { dg-message "should be initialized" }
};

template <class T> struct S2
{
  T const volatile i; // { dg-message "should be initialized" }
};

template <class T> struct S3
{
  T& ref; // { dg-message "should be initialized" }
};

template <class T> struct S4
{
  T const i; // { dg-message "should be initialized" }
  T& ref; // { dg-message "should be initialized" }
};

struct X
{
  X () : c (0), r (c) {}
  int const c;
  int const& r;
};

struct Y11
{
  int const i; // { dg-message "should be initialized" }
};

struct Y1
{
  Y11 a[1];
};

struct Y22
{
  int& ref; // { dg-message "should be initialized" }
};

struct Y2
{
  Y22 a[1];
};

struct Z1
{
  int const i; // { dg-message "should be initialized" }
};

struct Z2
{
  int& ref; // { dg-message "should be initialized" }
};

struct Z3
{
  int const i; // { dg-message "should be initialized" }
};

struct Z4
{
  int& ref; // { dg-message "should be initialized" }
};

struct Z5
{
  int i;
};

struct Z
{
  Z1 z1;
  Z2 z2;
  Z3 z3;
  Z4 z4;
  Z5 z5;
};

union U
{
  int const i; // { dg-message "should be initialized" }
};


void f1 ()
{
  A1 a1; // { dg-error "uninitialized const member" }
}

void f2 ()
{
  A2 a2; // { dg-error "uninitialized const member" }
}

void f3 ()
{
  A3 a3; // { dg-error "uninitialized reference member" }
}

void f4 ()
{
  A4 a4; // { dg-error "uninitialized reference member" }
}

void f5 ()
{
  A5 a5; // { dg-error "uninitialized reference member|uninitialized const member" }
}

void f6 ()
{
  S1<int> s; // { dg-error "uninitialized const member" }
}

void f7 ()
{
  S2<int> s; // { dg-error "uninitialized const member" }
}

void f8 ()
{
  S3<int> s; // { dg-error "uninitialized reference member" }
}

void f9 ()
{
  S4<int> s; // { dg-error "uninitialized reference member|uninitialized const member" }
}

void f10 ()
{
  X x;
}

void f11 ()
{
  A1 a[ 1 ]; // { dg-error "uninitialized const member" }
}

void f12 ()
{
  A3 a[ 1 ]; // { dg-error "uninitialized reference member" }
}

void f13 ()
{
  Y1 y1; // { dg-error "uninitialized const member" }
}

void f14 ()
{
  Y2 y2; // { dg-error "uninitialized reference member" }
}

void f15 ()
{
  Z z; // { dg-error "uninitialized reference member|uninitialized const member" }
}

void f16 ()
{
  U u; // { dg-error "uninitialized const member" }
}

