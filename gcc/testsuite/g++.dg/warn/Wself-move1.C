// PR c++/81159
// { dg-do compile { target c++11 } }
// { dg-options "-Wself-move" }

// Define std::move.
namespace std {
  template<typename _Tp>
    struct remove_reference
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&>
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&&>
    { typedef _Tp   type; };

  template<typename _Tp>
    constexpr typename std::remove_reference<_Tp>::type&&
    move(_Tp&& __t) noexcept
    { return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); }
}

int g;

struct S {
  int x;
  S(S&& o) {
    x = std::move (x); // { dg-warning "moving '\[^\n\r]*S::x' of type .int. to itself" }
    x = std::move (o.x);
    o.x = std::move (x);
    o.x = std::move (o.x); // { dg-warning "moving 'o.S::x' of type .int. to itself" }
  }
  void foo (int x) {
    x = std::move (x); // { dg-warning "moving 'x' of type .int. to itself" }
  }
};

struct X {
  int x;
  X(int x) : x(std::move (x)) { }
};

struct A {};
struct B { A a; };
struct C { C(); ~C(); };
struct D { D(); D(const D&); D(D&&); D& operator=(const D&); };

void
test ()
{
  int i = 42;
  i = std::move (i); // { dg-warning "moving 'i' of type .int. to itself" }
  (i) = std::move (i); // { dg-warning "moving 'i' of type .int. to itself" }

  g = std::move (g); // { dg-warning "moving 'g' of type .int. to itself" }
  (g) = std::move (g); // { dg-warning "moving 'g' of type .int. to itself" }

  A a;
  a = std::move (a); // { dg-warning "moving 'a' of type .A. to itself" }
  (a) = std::move (a); // { dg-warning "moving 'a' of type .A. to itself" }

  B b;
  b = std::move (b); // { dg-warning "moving 'b' of type .B. to itself" }
  (b) = std::move (b); // { dg-warning "moving 'b' of type .B. to itself" }
  b.a = std::move (b.a); // { dg-warning "moving 'b.B::a' of type .A. to itself" }
  (b.a) = std::move (b.a); // { dg-warning "moving 'b.B::a' of type .A. to itself" }

  C c;
  c = std::move (c); // { dg-warning "moving 'c' of type .C. to itself" }
  D d;
  d = std::move (d); // { dg-warning "moving 'd' of type .D. to itself" }
}

template<typename T>
void ttest ()
{
  T t;
  t = std::move (t); // { dg-warning "moving 't' of type .A. to itself" }
}

template void ttest<A>();

void
testref (int &r, int &&rr)
{
  r = std::move (r); // { dg-warning "moving 'r' of type .int. to itself" }
  rr = std::move (rr); // { dg-warning "moving 'rr' of type .int. to itself" }
}

// Test various other arguments to std::move.
template<typename T>
void
testargs (T *Tptr, T **Tpptr, T& Tref, T&& Trref, const T *Tcptr)
{
  Tptr = std::move (Tptr); // { dg-warning "moving 'Tptr' of type 'int\\*' to itself" }
  *Tptr = std::move (*Tptr); // { dg-warning "moving '\\* Tptr' of type 'int' to itself" }
  *Tptr = std::move (*(Tptr)); // { dg-warning "moving '\\* Tptr' of type 'int' to itself" }
  *(Tptr) = std::move (*Tptr); // { dg-warning "moving '\\* Tptr' of type 'int' to itself" }
  *(Tptr + 1) = std::move (*(Tptr + 1)); // { dg-warning "moving '\[^\n\r]*Tptr\[^\n\r]*' of type 'int' to itself" }
  *(Tptr + 1) = std::move (*(Tptr + 2));
  (*(Tptr)) = std::move (*Tptr); // { dg-warning "moving '\\* Tptr' of type 'int' to itself" }
  *Tpptr = std::move (*Tpptr); // { dg-warning "moving '\\* Tpptr' of type 'int\\*' to itself" }
  **Tpptr = std::move (**Tpptr); // { dg-warning "moving '\\* \\* Tpptr' of type 'int' to itself" }
  Tref = std::move (Tref); // { dg-warning "moving 'Tref' of type 'int' to itself" }
  Trref = std::move (Trref); // { dg-warning "moving 'Trref' of type 'int' to itself" }
  Tcptr = std::move (Tcptr); // { dg-warning "moving 'Tcptr' of type 'const int\\*' to itself" }
  (Tptr) = std::move (Tptr); // { dg-warning "moving 'Tptr' of type 'int\\*' to itself" }
  (*Tptr) = std::move (*Tptr); // { dg-warning "moving '\\* Tptr' of type 'int' to itself" }
  (*Tpptr) = std::move (*Tpptr); // { dg-warning "moving '\\* Tpptr' of type 'int\\*' to itself" }
  (**Tpptr) = std::move (**Tpptr); // { dg-warning "moving '\\* \\* Tpptr' of type 'int' to itself" }
  (*(*(Tpptr))) = std::move (**Tpptr); // { dg-warning "moving '\\* \\* Tpptr' of type 'int' to itself" }
  (Tref) = std::move (Tref); // { dg-warning "moving 'Tref' of type 'int' to itself" }
  (Trref) = std::move (Trref); // { dg-warning "moving 'Trref' of type 'int' to itself" }
  (Tcptr) = std::move (Tcptr); // { dg-warning "moving 'Tcptr' of type 'const int\\*' to itself" }
}

void
call_testargs ()
{
  int i = 42;
  int *p = &i;
  testargs<int>(&i, &p, i, 42, &i);
}
