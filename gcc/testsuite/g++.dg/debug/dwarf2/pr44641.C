// Origin: PR 44641
// { dg-do compile { target c++17_down } }
// { dg-options "-gdwarf-2 -O0 -dA" }

template <class A> struct MisplacedDbg;
template<class T> struct MisplacedDbg<T*>;
struct Full;
template<> struct MisplacedDbg<Full>;

struct Arg;
typedef MisplacedDbg<Arg> Typedef1;
typedef MisplacedDbg<Arg*> Typedef2;
typedef MisplacedDbg<Full> Typedef3;

template<typename T> struct Base  {
  int i;
  virtual ~Base() {
  }
};

template <>
struct MisplacedDbg<Full>  // { dg-function-on-line {_ZN12MisplacedDbgI4FullEC[12]Ev} { xfail powerpc-ibm-aix* } }
                           // { dg-function-on-line {_ZN12MisplacedDbgI4FullED0Ev} { target *-*-* xfail powerpc-ibm-aix* } 22 }

    : public Base<int> {
};

template <class T>
struct MisplacedDbg<T*>  // { dg-function-on-line {_ZN12MisplacedDbgIP3ArgEC[12]Ev} { xfail powerpc-ibm-aix* } }
                         // { dg-function-on-line {_ZN12MisplacedDbgIP3ArgED0Ev} { target *-*-* xfail powerpc-ibm-aix* } 29 }
    : public Base<int> {
};

template <class A>
struct MisplacedDbg  // { dg-function-on-line {_ZN12MisplacedDbgI3ArgEC[12]Ev} { xfail powerpc-ibm-aix* } }
                     // { dg-function-on-line {_ZN12MisplacedDbgI3ArgED0Ev} { target *-*-* xfail powerpc-ibm-aix* } 35 }
    : public Base<int> {
};

static MisplacedDbg<Arg> static_var1;
static MisplacedDbg<Arg*> static_var2;
static MisplacedDbg<Full> static_var3;

// This test is skipped in C++20 because we consider the default constructor
// MisplacedDbg() constexpr despite the uninitialized member "int i;".  So
// the calls to
//    MisplacedDbg<Arg>::MisplacedDbg()
//    MisplacedDbg<Full>::MisplacedDbg()
//    MisplacedDbg<Arg*>::MisplacedDbg()
// are elided.  (This comment is here not to mess up the line numbers.)
