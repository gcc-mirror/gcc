// PR c++/25811
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
  new A1; // { dg-error "uninitialized const member" }
}

void f2 ()
{
  new A2; // { dg-error "uninitialized const member" }
}

void f3 ()
{
  new A3; // { dg-error "uninitialized reference member" }
}

void f4 ()
{
  new A4; // { dg-error "uninitialized reference member" }
}

void f5 ()
{
  new A5; // { dg-error "uninitialized reference member|uninitialized const member" }
}

void f6 ()
{
  new S1<int>; // { dg-error "uninitialized const member" }
}

void f7 ()
{
  new S2<int>; // { dg-error "uninitialized const member" }
}

void f8 ()
{
  new S3<int>; // { dg-error "uninitialized reference member" }
}

void f9 ()
{
  new S4<int>; // { dg-error "uninitialized reference member|uninitialized const member" }
}

void f10 ()
{
  new X;
}

void f11 ()
{
  new A1[1]; // { dg-error "uninitialized const member" }
}

void f12 ()
{
  new A3[1]; // { dg-error "uninitialized reference member" }
}

void f13 ()
{
  new Y1; // { dg-error "uninitialized const member" }
}

void f14 ()
{
  new Y2; // { dg-error "uninitialized reference member" }
}

void f15 ()
{
  new Z; // { dg-error "uninitialized reference member|uninitialized const member" }
}

void f16 ()
{
  new U; // { dg-error "uninitialized const member" }
}
