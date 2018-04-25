// PR c++/60760 - arithmetic on null pointers should not be allowed
//     in constant expressions
// PR c++/71091 - constexpr reference bound to a null pointer dereference
//     accepted
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-pointer-arith" }

// Generate a null poiinter.
constexpr int* null () { return 0; }

// Test case from comment #0 in c++/60760.
namespace PR_60760_comment_0 {

constexpr int* ptr = nullptr;
constexpr int* ptr2 = ptr + 1;   // { dg-error "null pointer|not a constant" }

}

// Test case from comment #1 in c++/60760.
namespace PR_60760_comment_1 {

constexpr int* ptr = nullptr;

constexpr int zero = 0;
constexpr int* ptr2 = ptr + zero;   // Adding zero is valid.
constexpr int* ptr3 = ptr - zero;   // As is subtracting zero.

}

// Test case from c++/71091.
namespace PR_71091 {

constexpr int *p = 0;
constexpr const int &r = *p;   // { dg-error "dereferencing a null pointer" }

}

// Other test cases.
namespace C {

struct S { int a, b[1]; } s;

constexpr S *p0 = &s;
constexpr S *p1 = nullptr;
constexpr int *r0 = p1->b;   // { dg-error "null pointer|constant expression" }

// Adding and subtracting zero from and to a null pointer is valid.
constexpr S* r1 = p1 + 0;
constexpr S* r2 = r1 - 0;

constexpr int zero = 0;

constexpr S* r3 = r2 + zero;
constexpr S* r4 = r3 - zero;

static_assert (r4 == nullptr, "r4 == nullptr");

constexpr const S *pcs = p0;
constexpr int d1 = pcs - p0;
constexpr int d2 = p0 - pcs;

constexpr bool b = !p1 && !pcs;
}

namespace D {

struct A { int i; const A *pa1; const A *pa0; };

constexpr A a1 = { 0, 0, 0  };
constexpr A a2 = { 1, &a1, 0 };

constexpr const A *pa2 = &a2;
constexpr int i0 = pa2->i;
constexpr int i1 = pa2->pa1->i;
constexpr int i2 = pa2->pa1->pa0->i;	    // { dg-error "null pointer|not a constant" }

constexpr const A *pa3 = &*pa2->pa1->pa0;
constexpr const A *pa4 = pa2->pa1->pa0 + 1;     // { dg-error "null pointer|not a constant" }

constexpr const int *pi0 = &pa2->pa1->pa0->i;   // { dg-error "null pointer|not a constant" }

constexpr const A *pa5 = 0;
constexpr const int *pi1 = &pa5->i;	     // { dg-error "null pointer|not a constant" }

}


namespace SimpleTests {

constexpr int* p0 = nullptr;
constexpr int* q0 = p0;
constexpr int* r0 = null ();

// Conversion to cv-qualified void* is valid.
constexpr void* pv0 = p0;
constexpr const void* pv1 = p0;
constexpr volatile void* pv2 = p0;
constexpr const volatile void* pv3 = p0;
constexpr void* pv4 = static_cast<void*>(p0);
constexpr const void* pv5 = static_cast<const void*>(p0);

// The following was accepted due to bug c++/49171
constexpr void* pv6 = reinterpret_cast<void*>(p0);   // { dg-error "not a constant expression" }

// Adding or subtracting zero from a null pointer is valid in C++.
constexpr int* p1 = p0 + 0;
constexpr int* p2 = p0 - 0;
constexpr int* p3 = 0 + p0;

// While the text of the C++ standard still doesn't allow it, CWG
// issue 232 implies that dererencing a null pointer is intended
// to be permitted in contexts where the result isn't evaluated.
// For compatibility with C that should at a minimum include
// expressions like &*p that are valid there.
constexpr int* p4 = &*p0;
constexpr int* p5 = p0 + 1;       // { dg-error "null pointer|not a constant" }
constexpr int* p6 = 1 + p0;       // { dg-error "null pointer|not a constant" }
constexpr int* p7 = p0 - 1;       // { dg-error "null pointer|not a constant" }
constexpr int* p8 = &p0 [0];
constexpr int* p9 = &0 [p0];

constexpr int* p10 = null () + 2; // { dg-error "null pointer|not a constant" }
constexpr int* p11 = 3 + null (); // { dg-error "null pointer|not a constant" }
constexpr int* p12 = null () - 4; // { dg-error "null pointer|not a constant" }
constexpr int* p13 = &null ()[4]; // { dg-error "null pointer|not a constant" }
constexpr int* p14 = &3[null ()]; // { dg-error "null pointer|not a constant" }

constexpr int* q1 = q0 + 0;
constexpr int* q2 = q0 - 0;
constexpr int* q3 = q0 + 1;       // { dg-error "null pointer|not a constant" }
constexpr int* q4 = q0 + 2;       // { dg-error "null pointer|not a constant" }
constexpr int* q5 = &q0 [0];

// Subtracting null pointers from one another is valid.
constexpr int i0 = p0 - (int*)0;
constexpr int i1 = p0 - static_cast<int*>(0);
constexpr int i2 = p0 - (int*)nullptr;
constexpr int i3 = p0 - static_cast<int*>(nullptr);
constexpr int i4 = p0 - p0;
constexpr int i5 = p0 - q0;
constexpr int i6 = p0 - r0;
constexpr int i7 = (int*)0 - p0;
constexpr int i8 = static_cast<int*>(0) - p0;
constexpr int i9 = (int*)nullptr - p0;
constexpr int i10 = static_cast<int*>(nullptr) - p0;
constexpr int i11 = q0 - p0;
constexpr int i12 = r0 - p0;

}

namespace IncompleteTypeTests {

// The type must be complete.
struct X;
constexpr X *px0 = nullptr;
constexpr X *px1 = px0 + 0;     // { dg-error "invalid use of incomplete type"  }
constexpr X *px2 = px0 - 0;     // { dg-error "invalid use of incomplete type"  }
constexpr X *px3 = px0 - px0;   // { dg-error "invalid use of incomplete type"  }

constexpr void *pv0 = px0;
constexpr void *pv1 = pv0;
constexpr const void *pv2 = pv0;
constexpr void *pv3 = pv2;      // { dg-error "invalid conversion|not a constant expression" }
constexpr const void *pv4 = pv2;

constexpr X *px4 = pv0;	 // { dg-error "invalid conversion|not a constant expression" }

}

namespace IndirectTests {

struct S { int i, j; struct SA { struct SB { int *pi; } sb; } sa; };

constexpr S* ps = (S*)0;

// Comparing null pointers is valid.
constexpr bool b0 = ps == ps;
constexpr bool b1 = ps != ps;
constexpr bool b2 = ps <  ps;
constexpr bool b3 = ps <= ps;
constexpr bool b4 = ps >  ps;
constexpr bool b5 = ps >= ps;

constexpr bool b6 = ps == (S*)0;
constexpr bool b7 = ps != (S*)0;
constexpr bool b8 = ps <  (S*)0;
constexpr bool b9 = ps <= (S*)0;
constexpr bool b10 = ps >  (S*)0;
constexpr bool b11 = ps >= (S*)0;

constexpr S* ps1 = ps;
constexpr S* ps2 = ps1;

constexpr int* pi0 = &((S*)0)->i;	// { dg-error "null pointer|not a constant" }
constexpr int* pi1 = &((S*)nullptr)->i;	// { dg-error "null pointer|not a constant" }

constexpr int* pj0 = &((S*)0)->j;	// { dg-error "null pointer|not a constant" }
constexpr int* pj1 = &((S*)nullptr)->j;	// { dg-error "null pointer|not a constant" }

constexpr int* psi = &ps->i;	    // { dg-error "null pointer|not a constant" }
constexpr int* psj = &ps->j;	    // { dg-error "null pointer|not a constant" }

constexpr int* ps1i = &ps1->i;	  // { dg-error "null pointer|not a constant" }
constexpr int* ps2i = &ps1->i;	  // { dg-error "null pointer|not a constant" }

constexpr int* ps1j = &ps1->j;	  // { dg-error "null pointer|not a constant" }
constexpr int* ps2j = &ps1->j;	  // { dg-error "null pointer|not a constant" }

}

namespace BaseAndDerivedTests {

struct A { };
struct B: A { };
struct C: B { };
struct D: B, C { };		     // { dg-warning "inaccessible" }

constexpr D *pd0 = 0;
constexpr C *pc0 = 0;
constexpr B *pb0 = 0;

constexpr A *pa0 = pb0;
constexpr A *pa1 = static_cast<A*>(pb0);
constexpr A *pa2 = pc0;
constexpr A *pa3 = pd0;		   // { dg-error "ambiguous base" }
constexpr A *pa4 = static_cast<A*>(pd0);  // { dg-error "ambiguous base" }

constexpr B *pb1 = pa0;		   // { dg-error "invalid conversion|not a constant expression" }
constexpr B *pb2 = static_cast<B*>(pa0);  // { dg-error "not a constant expression" }

constexpr C *pc1 = pa0;		   // { dg-error "invalid conversion|not a constant expression" }
constexpr D *pd1 = pa0;		   // { dg-error "ambiguous base|invalid conversion" }

struct E: private A { };

constexpr E *pe0 = 0;
constexpr A *pa5 = pe0;		 // { dg-error "inaccessible base of" }

struct VA { virtual ~VA (); };
struct VB: virtual VA { };
struct VC: virtual VA { };
struct VD: VB, VC { };

constexpr VD *pvd0 = 0;
constexpr VC *pvc0 = 0;
constexpr VB *pvb0 = 0;

constexpr VA *pva0 = pvb0;
constexpr VA *pva1 = pvc0;
constexpr VA *pva2 = pvd0;

constexpr VB *pvb1 = pva0;	      // { dg-error "invalid conversion|cannot convert from pointer to base class" }

}

namespace FunctionTests {

typedef void Func ();

// Arithmetic on member function pointers is diagnosed with -Wpointer-arith.
// With constexpr, only zero may be added or subtracted.
constexpr Func *pf0 = 0;
constexpr Func *pf1 = pf0 + 0;  // triggers -Wpointer-arith
constexpr Func *pf2 = pf0 - 0;  // triggers -Wpointer-arith
constexpr Func *pf3 = 0 + pf0;  // triggers -Wpointer-arith
constexpr Func *pf4 = pf0 + 1;  // { dg-error "null pointer|not a constant" }
constexpr Func *pf5 = 2 + pf0;  // { dg-error "null pointer|not a constant" }
constexpr Func *pf6 = pf0 - 3;  // { dg-error "null pointer|not a constant" }

struct S;
typedef void (S::*MemFuncPtr)();

// Arithmetic on member function pointers is rejected with a hard error.
constexpr MemFuncPtr pmf0 = nullptr;
constexpr MemFuncPtr pmf1 = pmf0 + 0;   // { dg-error "invalid operands" }
constexpr MemFuncPtr pmf2 = 0 + pmf0;   // { dg-error "invalid operands" }
constexpr MemFuncPtr pmf3 = pmf0 + 1;   // { dg-error "invalid operands" }
constexpr MemFuncPtr pmf4 = 1 + pmf0;   // { dg-error "invalid operands" }
constexpr MemFuncPtr pmf5 = pmf0 - 1;   // { dg-error "invalid operands" }

}

namespace ConversionTest {

struct A {
  int *p;
};

constexpr const int* f (const int *p) { return p; }

void f ()
{
  static_assert (!f (0), "f (a.p)");
  static_assert (!f (nullptr), "f (a.p)");

  constexpr A a = A ();

  static_assert (!f (a.p), "f (a.p)");
}

}
