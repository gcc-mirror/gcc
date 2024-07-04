// DR 2627 - Bit-fields and narrowing conversions
// PR c++/94058
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-error=narrowing" }

using int64_t = __INT64_TYPE__;
using int32_t = __INT32_TYPE__;

struct A {
  int64_t i1 : __CHAR_BIT__;
  int64_t i2 : sizeof (int32_t) * __CHAR_BIT__ - 1;
  int64_t i3 : sizeof (int32_t) * __CHAR_BIT__;
  int64_t i4 : sizeof (int32_t) * __CHAR_BIT__ + 1;
  int64_t i5 : sizeof (int64_t) * __CHAR_BIT__ - 1;
  int64_t i6 : sizeof (int64_t) * __CHAR_BIT__;
} a;

int32_t i1{a.i1};
int32_t i2{a.i2};
int32_t i3{a.i3};
int32_t i4{a.i4}; // { dg-warning "narrowing conversion" }
int32_t i5{a.i5}; // { dg-warning "narrowing conversion" }
int32_t i6{a.i6}; // { dg-warning "narrowing conversion" }

struct B {
  bool b1 : sizeof (bool) * __CHAR_BIT__;
  bool b2 : sizeof (bool);
} b;

signed char b1{b.b1};
signed char b2{b.b2};

enum E : int64_t { E1 };

struct C {
  E e1 : __CHAR_BIT__;
  E e2 : sizeof (int32_t) * __CHAR_BIT__ - 1;
  E e3 : sizeof (int32_t) * __CHAR_BIT__;
  E e4 : sizeof (int32_t) * __CHAR_BIT__ + 1;
  E e5 : sizeof (int64_t) * __CHAR_BIT__ - 1;
  E e6 : sizeof (int64_t) * __CHAR_BIT__;
} c;

int32_t e1{c.e1};
int32_t e2{c.e2};
int32_t e3{c.e3};
int32_t e4{c.e4}; // { dg-warning "narrowing conversion" }
int32_t e5{c.e5}; // { dg-warning "narrowing conversion" }
int32_t e6{c.e6}; // { dg-warning "narrowing conversion" }
