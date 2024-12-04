// P2662R3 - Pack Indexing
// PR c++/113798
// { dg-do compile { target c++26 } }
// Test invalid cases.

template<int I, typename... Ts>
using Type = Typo...[I]; // { dg-error "does not name a type" }

template<int I, auto... Ts>
constexpr auto Var = Typo...[I]; // { dg-error "no parameter packs" }

template<typename... Ts>
void foo(Ts...[]);  // { dg-error "pack index missing" }

template <typename... Ts>
void f(Ts...[1]);

template<int... N>
int f2 (X... [N]); // { dg-error "contains no parameter packs" }

struct X;

template<typename T>
struct TX;

template <typename T, auto V, template<typename> typename Tp>
void
bad (int i)
{
  i...[0];  // { dg-error "no parameter packs" }
  V...[0];  // { dg-error "no parameter packs" }
  X...[0] x;  // { dg-error "no parameter packs" }
  T...[0] t;  // { dg-error "no parameter packs" }
  Tp...[0] tp;  // { dg-error "expected" }

  X...[0] xarr[1];  // { dg-error "no parameter packs" }
  T...[0] tarr[1];  // { dg-error "no parameter packs" }
  Tp...[0] tparr[1];  // { dg-error "expected" }
}

template<int N>
int
getT (auto... Ts)
{
  return Ts...[N]; // { dg-error "cannot index an empty pack" }
}

template<int N>
int
getT2 (auto... Ts)
{
  return Ts...[N]; // { dg-error "pack index is negative" }
}

template<auto N, typename... Ts>
void
badtype ()
{
  Ts...[N] t; // { dg-error "cannot index an empty pack" }
}

template<auto N, typename... Ts>
void
badtype2 ()
{
  Ts...[N] t; // { dg-error "pack index is out of range" }
}

template<auto N, typename... Ts>
void
badtype3 ()
{
  Ts...[N] t; // { dg-error "cannot index an empty pack" }
}

template<auto N, typename... Ts>
void
badtype4 ()
{
  Ts...[N] t; // { dg-error "pack index is negative" }
}

int nonconst () { return 42; }

template<typename... Ts>
void
badindex ()
{
  Ts...[nonconst ()] t; // { dg-error "pack index is not an integral constant" }
  // { dg-error "non-.constexpr. function" "" { target *-*-* } .-1 }
}

template<typename... Ts>
struct broken {
  Ts...1;  // { dg-error "expected" }
  Ts...[;  // { dg-error "invalid" }
  Ts...[1;  // { dg-error "invalid" }
  Ts...[];  // { dg-error "pack index missing" }

  void foo (auto...Vs) {
    decltype(Vs...[1]) d1 = Vs...[]; // { dg-error "pack index missing" }
    decltype(Vs...[1]) d2 = Vs...[; // { dg-error "expected" }
  }
};

int main()
{
   // void f<int, double>(int [1], double [1])
  f<int, double>(nullptr, nullptr); // { dg-error "no matching function" }
  bad<int, 0, TX>(42);

  getT<0>(); // { dg-message "required from here" }
  getT<1>();  // { dg-message "required from here" }
  getT2<-1>(1);  // { dg-message "required from here" }

  badtype<0>(); // { dg-message "required from here" }
  badtype2<1, int>(); // { dg-message "required from here" }
  badtype3<-1>(); // { dg-message "required from here" }
  badtype4<-1, int>(); // { dg-message "required from here" }

  badindex<int, int, int>();

  bool b = nothere...[0]; // { dg-error "no parameter packs" }
  using E = nothere...[0]; // { dg-error "does not name a type" }
}
