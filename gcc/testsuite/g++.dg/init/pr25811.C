// PR c++/25811
// { dg-do compile }

struct A1 // { dg-message "implicitly deleted" "" { target c++11 } }
          // { dg-error "uninitialized" "" { target c++11 } 4 }
{
  int const j; // { dg-message "should be initialized" }
};

struct A2 // { dg-message "implicitly deleted" "" { target c++11 } }
          // { dg-error "uninitialized" "" { target c++11 } 10 }
{
  int const volatile i; // { dg-message "should be initialized" }
};

struct A3 // { dg-message "implicitly deleted" "" { target c++11 } }
	  // { dg-error "uninitialized" "" { target c++11 } 16 }
{
  int& ref; // { dg-message "should be initialized" }
};

struct A4 // { dg-message "implicitly deleted" "" { target c++11 } }
	  // { dg-error "uninitialized" "" { target c++11 } 22 }
{
  int const& ref; // { dg-message "should be initialized" }
};

struct A5 // { dg-message "implicitly deleted" "" { target c++11 } }
	  // { dg-error "uninitialized" "" { target c++11 } 28 }
{
  int& ref; // { dg-message "should be initialized" }
  int const i; // { dg-message "should be initialized" }
};

template <class T> struct S1 // { dg-message "implicitly deleted" "" { target c++11 } }
			     // { dg-error "uninitialized" "" { target c++11 } 35 }
{
  T const i; // { dg-message "should be initialized" }
};

template <class T> struct S2 // { dg-message "implicitly deleted" "" { target c++11 } }
			     // { dg-error "uninitialized" "" { target c++11 } 41 }
{
  T const volatile i; // { dg-message "should be initialized" }
};

template <class T> struct S3 // { dg-message "implicitly deleted" "" { target c++11 } }
			     // { dg-error "uninitialized" "" { target c++11 } 47 }
{
  T& ref; // { dg-message "should be initialized" }
};

template <class T> struct S4 // { dg-message "implicitly deleted" "" { target c++11 } }
			     // { dg-error "uninitialized" "" { target c++11 } 53 }
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

struct Y11 // { dg-message "implicitly deleted" "" { target c++11 } }
	   // { dg-error "uninitialized" "" { target c++11 } 67 }
{
  int const i; // { dg-message "should be initialized" }
};

struct Y1 // { dg-error "deleted" "" { target c++11 } }
{
  Y11 a[1];
};

struct Y22 // { dg-message "implicitly deleted" "" { target c++11 } }
	   // { dg-error "uninitialized" "" { target c++11 } 78 }
{
  int& ref; // { dg-message "should be initialized" }
};

struct Y2 // { dg-error "deleted" "" { target c++11 } }
{
  Y22 a[1];
};

struct Z1 // { dg-message "implicitly deleted" "" { target c++11 } }
	  // { dg-error "uninitialized" "" { target c++11 } 89 }
{
  int const i; // { dg-message "should be initialized" }
};

struct Z2 // { dg-message "implicitly deleted" "" { target c++11 } }
	  // { dg-error "uninitialized" "" { target c++11 } 95 }
{
  int& ref; // { dg-message "should be initialized" }
};

struct Z3 // { dg-message "implicitly deleted" "" { target c++11 } }
	  // { dg-error "uninitialized" "" { target c++11 } 101 }
{
  int const i; // { dg-message "should be initialized" }
};

struct Z4 // { dg-message "implicitly deleted" "" { target c++11 } }
	  // { dg-error "uninitialized" "" { target c++11 } 107 }
{
  int& ref; // { dg-message "should be initialized" }
};

struct Z5
{
  int i;
};

struct Z // { dg-error "deleted" "" { target c++11 } }
{
  Z1 z1;
  Z2 z2;
  Z3 z3;
  Z4 z4;
  Z5 z5;
};

union U // { dg-message "implicitly deleted" "" { target c++11 } }
	// { dg-error "uninitialized" "" { target c++11 } 127 }
{
  int const i; // { dg-message "should be initialized" }
};

void f1 ()
{
  new A1; // { dg-error "deleted|uninitialized const member" }
}

void f2 ()
{
  new A2; // { dg-error "deleted|uninitialized const member" }
}

void f3 ()
{
  new A3; // { dg-error "deleted|uninitialized reference member" }
}

void f4 ()
{
  new A4; // { dg-error "deleted|uninitialized reference member" }
}

void f5 ()
{
  new A5; // { dg-error "deleted|uninitialized reference member|uninitialized const member" }
}

void f6 ()
{
  new S1<int>; // { dg-error "deleted|uninitialized const member" }
}

void f7 ()
{
  new S2<int>; // { dg-error "deleted|uninitialized const member" }
}

void f8 ()
{
  new S3<int>; // { dg-error "deleted|uninitialized reference member" }
}

void f9 ()
{
  new S4<int>; // { dg-error "deleted|uninitialized reference member|uninitialized const member" }
}

void f10 ()
{
  new X;
}

void f11 ()
{
  new A1[1]; // { dg-error "deleted|uninitialized const member" }
}

void f12 ()
{
  new A3[1]; // { dg-error "deleted|uninitialized reference member" }
}

void f13 ()
{
  new Y1; // { dg-error "deleted|uninitialized const member" }
}

void f14 ()
{
  new Y2; // { dg-error "deleted|uninitialized reference member" }
}

void f15 ()
{
  new Z; // { dg-error "deleted|uninitialized reference member|uninitialized const member" }
}

void f16 ()
{
  new U; // { dg-error "deleted|uninitialized const member" }
}
