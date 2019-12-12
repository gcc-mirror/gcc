// PR c++/67376 - [5/6 regression] Comparison with pointer to past-the-end
//                of array fails inside constant expression
// This test verifies the aspect of the bug raised in comment #10,
// specifically comparing pointers to null.  The basic regression test
// is in g++.dg/cpp0x/constexpr-67376.C.
// Note also that while the description of the bug talks about pointers
// pointing past the end of arrays but the prolem is more general than
// that and involves all constexpr object pointers.

// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wall -Wextra -fdelete-null-pointer-checks" }

namespace A {

extern int i;

constexpr int *p0 = &i;

constexpr bool b0  = p0;        // { dg-warning "address of .A::i." }
constexpr bool b1  = p0 == 0;   // { dg-warning "address of .A::i." }
constexpr bool b2  = p0 != 0;   // { dg-warning "address of .A::i." }
constexpr bool b3  = p0 <  0;   // { dg-warning "25:ordered comparison" }
constexpr bool b4  = p0 <= 0;   // { dg-warning "25:ordered comparison" }
constexpr bool b5  = p0 >  0;   // { dg-warning "25:ordered comparison" }
constexpr bool b6  = p0 >= 0;   // { dg-warning "25:ordered comparison" }

constexpr bool b7  = !p0;       // { dg-warning "address of .A::i." }
constexpr bool b8  = 0 == p0;   // { dg-warning "address of .A::i." }
constexpr bool b9  = 0 != p0;   // { dg-warning "address of .A::i." }
constexpr bool b10 = 0 <  p0;   // { dg-warning "24:ordered comparison" }
constexpr bool b11 = 0 <= p0;   // { dg-warning "24:ordered comparison" }
constexpr bool b12 = 0 >  p0;   // { dg-warning "24:ordered comparison" }
constexpr bool b13 = 0 >= p0;   // { dg-warning "24:ordered comparison" }

}

namespace B {

// PR c++/70172 - incorrect reinterpret_cast from integer to pointer
// error on invalid constexpr initialization

struct S { int a, b[1]; } s;

constexpr S *p0 = &s;

constexpr int *q0 = p0->b;      // { dg-bogus "reinterpret_cast from integer to pointer" }

}

namespace WeakRefTest1 {

extern __attribute__ ((weak)) int i;

constexpr int *p0 = &i;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wextra"
// Suppress warning: ordered comparison of pointer with integer zero

constexpr bool b0  = p0;        // { dg-error "not a constant expression" }
constexpr bool b1  = p0 == 0;   // { dg-error "not a constant expression" }
constexpr bool b2  = p0 != 0;   // { dg-error "not a constant expression" }
constexpr bool b4  = p0 <= 0;   // { dg-error "not a constant expression" }
constexpr bool b5  = p0 >  0;   // { dg-error "not a constant expression" }

constexpr bool b7  = !p0;       // { dg-error "not a constant expression" }
constexpr bool b8  = 0 == p0;   // { dg-error "not a constant expression" }
constexpr bool b9  = 0 != p0;   // { dg-error "not a constant expression" }
constexpr bool b10 = 0 <  p0;   // { dg-error "not a constant expression" }
constexpr bool b13 = 0 >= p0;   // { dg-error "not a constant expression" }

// The following are accepted as constant expressions due to bug c++/70196.
constexpr bool b3  = p0 <  0;
constexpr bool b6  = p0 >= 0;
constexpr bool b11 = 0 <= p0;
constexpr bool b12 = 0 >  p0;

#pragma GCC diagnostic pop

}

namespace WeakRefTest2 {

extern __attribute__ ((weak)) int i;

constexpr int *p1 = &i + 1;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wextra"
// Suppress warning: ordered comparison of pointer with integer zero

constexpr bool b0  = p1;        // { dg-error "not a constant expression" }
constexpr bool b1  = p1 == 0;   // { dg-error "not a constant expression" }
constexpr bool b2  = p1 != 0;   // { dg-error "not a constant expression" }
constexpr bool b4  = p1 <= 0;   // { dg-error "not a constant expression" }
constexpr bool b5  = p1 >  0;   // { dg-error "not a constant expression" }

constexpr bool b7  = !p1;       // { dg-error "not a constant expression" }
constexpr bool b8  = 0 == p1;   // { dg-error "not a constant expression" }
constexpr bool b9  = 0 != p1;   // { dg-error "not a constant expression" }
constexpr bool b10 = 0 <  p1;   // { dg-error "not a constant expression" }
constexpr bool b13 = 0 >= p1;   // { dg-error "not a constant expression" }

// The following are accepted as constant expressions due to bug c++/70196.
// constexpr bool b3  = p1 <  0;
// constexpr bool b6  = p1 >= 0;
// constexpr bool b11 = 0 <= p1;
// constexpr bool b12 = 0 >  p1;

#pragma GCC diagnostic pop

}
