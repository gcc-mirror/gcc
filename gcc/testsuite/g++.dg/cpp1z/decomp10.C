// { dg-do compile { target c++17 } }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct A1 { int i,j; } a1;
template<> struct std::tuple_size<A1> {  };
void f1() { auto [ x ] = a1; }	// { dg-error "is not an integral constant expression" }

struct A2 { int i,j; } a2;
template<> struct std::tuple_size<A2> { enum { value = 5 }; };
void f2() { auto [ x ] = a2; }	// { dg-error "only 1 name provided" }
			        // { dg-message "decomposes into 5" "" { target *-*-* } .-1 }

struct A3 { int i,j; } a3;
template<> struct std::tuple_size<A3> { enum { value = 1 }; };
void f3() { auto [ x ] = a3; }	// { dg-error "get" }

struct A3a { int i,j; int get(); } a3a;
template<> struct std::tuple_size<A3a> { enum { value = 1 }; };
void f3a() { auto [ x ] = a3a; }	// { dg-error "get" }

struct A3b { int i,j; } a3b;
int get(A3b&&);
template<> struct std::tuple_size<A3b> { enum { value = 1 }; };
void f3b() { auto [ x ] = a3b; }	// { dg-error "get<0>" }

struct A4 {
  int ar[3];
  template <int I> int& get() { return ar[I]; }
} a4;
template<> struct std::tuple_size<A4> { enum { value = 3 }; };
void f4() { auto [ x, y, z ] = a4; }	// { dg-error "tuple_element" }

struct A5 { } a5;
template <int I> int& get(A5&& a);
template<> struct std::tuple_size<A5> { enum { value = 3 }; };
void f5() { auto [ x, y, z ] = a5; }	// { dg-error "tuple_element" }

struct A6 { } a6;
template <int I> int& get(A6&& a);
template<> struct std::tuple_size<A6> { enum { value = 3 }; };
template<> struct std::tuple_element<0, A6> { };
void f6() { auto [ x, y, z ] = a6; }	// { dg-error "no type named .type" }
