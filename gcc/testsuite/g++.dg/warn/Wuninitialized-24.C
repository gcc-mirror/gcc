// PR c++/19808
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized -Winit-self" }

int fint(int);
int fintp(int *);
int fintr(int &);
int fintcr(const int &);

int arr[10];

struct S {
  int x;
  int y;
  const int z = 42;
  int *p;

  S(int (*)[1]) : x(x) { } // { dg-warning "initialized with itself" }
  S(int (*)[2]) : x(x + x) { } // { dg-warning "member .S::x. is used uninitialized" }
  S(int (*)[3]) : x(static_cast<int>(y)) { } // { dg-warning "member .S::y. is used uninitialized" }
  S(int (*)[4]) : x(static_cast<int>(x)) { } // { dg-warning "member .S::x. is used uninitialized" }
  S(int (*)[5]) : x(fint(x)) { }
  S(int (*)[6]) : x(fint(y)) { }

  S(int (*)[7]) : x(sizeof(x)) { }
  S(int (*)[8]) : x(sizeof(y)) { }
  S(int (*)[9]) : p(&x) { }
  S(int (*)[10]) : x(fintp(&y)) { }
  S(int (*)[11]) : x(fintr(y)) { }
  S(int (*)[12]) : x(fintcr(y)) { }
  S(int (*)[26]) : x(((void)(__typeof(y)) 1, 1)) { }
  S(int (*)[27]) : x(((void)(decltype(y)) 1, 1)) { }
  S(int (*)[28]) : x(__alignof__(y)) { }
  S(int (*)[29]) : x(noexcept(y)) { }

  S(int (*)[13]) : x(0), y(x ? y : y) { }
  S(int (*)[14]) : x(0), y(1 + (x ? y : y)) { }
  S(int (*)[15]) : x(-y) { } // { dg-warning "member .S::y. is used uninitialized" }
  S(int (*)[16]) : x(1 << y) { } // { dg-warning "member .S::y. is used uninitialized" }
  S(int (*)[17]) : x(this->y) { } // { dg-warning "member .S::y. is used uninitialized" }
  S(int (*)[18]) : x(arr[y]) { } // { dg-warning "member .S::y. is used uninitialized" }
  S(int (*)[19]) : x(0), y(x ? x : y) { }
  S(int (*)[20]) : x(0), y(y ? x : y) { }
  S(int (*)[21]) : x(0), y(y ? x : x) { }
  S(int (*)[22]) : x(0), y((fint(y), x)) { }
  S(int (*)[23]) : x(0), y(x += y) { } // "member .S::y. is used uninitialized" but too complex for the FE
  S(int (*)[24]) : x(y += 10) { } // "member .S::y. is used uninitialized" but too complex for the FE
  S(int (*)[25]) : x(y++) { } // { dg-warning "member .S::y. is used uninitialized" }
};

// Same, but { }.
struct R {
  int x;
  int y;
  const int z = 42;
  int *p;

  R(int (*)[1]) : x{x} { } // { dg-warning "member .R::x. is used uninitialized" }
  R(int (*)[2]) : x{x + x} { } // { dg-warning "member .R::x. is used uninitialized" }
  R(int (*)[3]) : x{static_cast<int>(y)} { } // { dg-warning "member .R::y. is used uninitialized" }
  R(int (*)[4]) : x{static_cast<int>(x)} { } // { dg-warning "member .R::x. is used uninitialized" }
  R(int (*)[5]) : x{fint(x)} { }
  R(int (*)[6]) : x{fint(y)} { }

  R(int (*)[7]) : x{sizeof(x)} { }
  R(int (*)[8]) : x{sizeof(y)} { }
  R(int (*)[9]) : p{&x} { }
  R(int (*)[10]) : x{fintp(&y)} { }
  R(int (*)[11]) : x{fintr(y)} { }
  R(int (*)[12]) : x{fintcr(y)} { }
  R(int (*)[26]) : x{((void)(__typeof(y)) 1, 1)} { }
  R(int (*)[27]) : x{((void)(decltype(y)) 1, 1)} { }
  R(int (*)[28]) : x{__alignof__(y)} { }
  R(int (*)[29]) : x{noexcept(y)} { }

  R(int (*)[13]) : x{0}, y{x ? y : y} { }
  R(int (*)[14]) : x{0}, y{1 + (x ? y : y)} { }
  R(int (*)[15]) : x{-y} { } // { dg-warning "member .R::y. is used uninitialized" }
  R(int (*)[16]) : x{1 << y} { } // { dg-warning "member .R::y. is used uninitialized" }
  R(int (*)[17]) : x{this->y} { } // { dg-warning "member .R::y. is used uninitialized" }
  R(int (*)[18]) : x{arr[y]} { } // { dg-warning "member .R::y. is used uninitialized" }
  R(int (*)[19]) : x{0}, y{x ? x : y} { }
  R(int (*)[20]) : x{0}, y{y ? x : y} { }
  R(int (*)[21]) : x{0}, y{y ? x : x} { }
  R(int (*)[22]) : x{0}, y{(fint(y), x)} { }
  R(int (*)[23]) : x{0}, y{x += y} { } // "member .R::y. is used uninitialized" but too complex for the FE
  R(int (*)[24]) : x{y += 10} { } // "member .R::y. is used uninitialized" but too complex for the FE
  R(int (*)[25]) : x{y++} { } // { dg-warning "member .R::y. is used uninitialized" }
};
