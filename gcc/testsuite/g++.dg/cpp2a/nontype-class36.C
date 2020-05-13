/* PR c++/94510 - nullptr_t implicitly cast to zero twice in std::array
   { dg-do compile { target c++20 } }
   { dg-options "-Wall" } */

struct A { int i; int f (); };
typedef int A::*MemPtr;
typedef int (A::*MemFuncPtr)();

struct B { MemPtr a[3]; MemFuncPtr b[3]; };

static const constexpr MemPtr mp0 = { 0 };
static const constexpr MemPtr mpn = { nullptr };
static const constexpr MemPtr mp_ = { };
static const constexpr MemPtr mpi = { &A::i };

template <B> struct X { };

typedef X<B{ }>                               XB;
typedef X<B{ 0 }>                             XB;
typedef X<B{{ 0 }}>                           XB;
typedef X<B{{ MemPtr{ }}}>                    XB;
typedef X<B{{ MemPtr{ 0 }}}>                  XB;
typedef X<B{{ MemPtr () }}>                   XB;
typedef X<B{{ MemPtr{ nullptr }}}>            XB;
typedef X<B{{ mp_ }}>                         XB;
typedef X<B{{ mpn }}>                         XB;
typedef X<B{{ mp0 }}>                         XB;

typedef X<B{ mpi }>                           XBp;
typedef X<B{ mpi, 0 }>                        XBp;
typedef X<B{{ mpi, 0 }}>                      XBp;
typedef X<B{{ mpi, MemPtr{ }}}>               XBp;
typedef X<B{{ mpi, MemPtr{ 0 }}}>             XBp;
typedef X<B{{ mpi, MemPtr () }}>              XBp;
typedef X<B{{ mpi, MemPtr{ nullptr }}}>       XBp;
typedef X<B{{ mpi, mp_ }}>                    XBp;
typedef X<B{{ mpi, mpn }}>                    XBp;
typedef X<B{{ mpi, mp0 }}>                    XBp;

typedef X<B{ mpi, mpi }>                      XBpp;
typedef X<B{ mpi, mpi, 0 }>                   XBpp;
typedef X<B{{ mpi, mpi, 0 }}>                 XBpp;
typedef X<B{{ mpi, mpi, MemPtr{ }}}>          XBpp;
typedef X<B{{ mpi, mpi, MemPtr{ 0 }}}>        XBpp;
typedef X<B{{ mpi, mpi, MemPtr () }}>         XBpp;
typedef X<B{{ mpi, mpi, MemPtr{ nullptr }}}>  XBpp;
typedef X<B{{ mpi, mpi, mp_ }}>               XBpp;
typedef X<B{{ mpi, mpi, mpn }}>               XBpp;
typedef X<B{{ mpi, mpi, mp0 }}>               XBpp;

typedef X<B{ 0, mpi }>                        XB0p;
typedef X<B{ nullptr, mpi, 0 }>               XB0p;
typedef X<B{ mp0, mpi, 0 }>                   XB0p;

typedef X<B{ 0, 0, mpi }>                     XB00p;
typedef X<B{ 0, nullptr, mpi }>               XB00p;
typedef X<B{ nullptr, 0, mpi }>               XB00p;
typedef X<B{ nullptr, nullptr, mpi }>         XB00p;
typedef X<B{ MemPtr{ }, MemPtr{ }, mpi }>     XB00p;
typedef X<B{ mp0, MemPtr{ }, mpi }>           XB00p;
typedef X<B{ mpn, mpn, mpi }>                 XB00p;
typedef X<B{ mpn, mp_, mpi }>                 XB00p;  // { dg-bogus "conflicting declaration" "pr94568" { xfail *-*-* } }

static const constexpr MemFuncPtr mfp0 = { 0 };
static const constexpr MemFuncPtr mfpn = { nullptr };
static const constexpr MemFuncPtr mfp_ = { };

typedef X<B{{ }, { }}>                        XB;
typedef X<B{{ }, { 0 }}>                      XB;
typedef X<B{{ }, { MemFuncPtr{ }}}>           XB;
typedef X<B{{ }, { MemFuncPtr{ 0 }}}>         XB;
typedef X<B{{ }, { MemFuncPtr () }}>          XB;
typedef X<B{{ }, { MemFuncPtr{ nullptr }}}>   XB;
typedef X<B{{ }, { mfp_ }}>                   XB;
typedef X<B{{ }, { mfpn }}>                   XB;
typedef X<B{{ }, { mfp0 }}>                   XB;
