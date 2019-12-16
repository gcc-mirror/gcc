// { dg-options "-Wconditionally-supported" }

// DR 195 was about allowing conversions between function and object
// pointers under some circumstances.  The issue got resolved for C++11,
// which, in 5.2.10 p8 says that: "Converting a function pointer to an 
// object pointer type or vice versa is conditionally-supported."

// This checks we warn with -Wconditionally-supported.

typedef void (*PF)(void);
typedef void *PV;
typedef int *PO;

void foo ()
{
  PF pf;
  PV pv;
  PO po;

  pf = reinterpret_cast <PF>(pv); // { dg-warning "8:casting between pointer-to-function and pointer-to-object is conditionally-supported" }
  pv = reinterpret_cast <PV>(pf); // { dg-warning "8:casting between pointer-to-function and pointer-to-object is conditionally-supported" }

  pf = reinterpret_cast <PF>(po); // { dg-warning "8:casting between pointer-to-function and pointer-to-object is conditionally-supported" }
  po = reinterpret_cast <PO>(pf); // { dg-warning "8:casting between pointer-to-function and pointer-to-object is conditionally-supported" }
}
