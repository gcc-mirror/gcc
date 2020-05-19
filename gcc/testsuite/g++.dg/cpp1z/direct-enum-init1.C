// P0138R2 - direct-list-initialization of enums
// { dg-do compile { target c++11 } }

enum A { G = 26 };
enum B : short {};
enum class C {};
enum struct D : long {};
enum class E : unsigned char { e = 7 };
struct S { operator C () { return C (s); } int s; } s;
struct T { operator long () { return t; } long t; } t;
struct V { E v; };
long l;
long long ll;
short c;
void bar (E);

void
foo ()
{
  A a1 { 5 };		// { dg-error "invalid conversion from 'int' to 'A'" }
  B b1 { 7 };		// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
  C c1 { s };
  D d1 { D(t) };	// { dg-error "invalid cast from type 'T' to type 'D'" }
  D d2 { t };		// { dg-error "cannot convert 'T' to 'D' in initialization" "" { target c++14_down } }
			// { dg-error "invalid cast from type 'T' to type 'D'" "" { target c++17 } .-1 }
  D d3 { 9 };		// { dg-error "cannot convert 'int' to 'D' in initialization" "" { target c++14_down } }
  D d4 { l };		// { dg-error "cannot convert 'long int' to 'D' in initialization" "" { target c++14_down } }
  D d5 { D(l) };
  D d6 { G };		// { dg-error "cannot convert 'A' to 'D' in initialization" "" { target c++14_down } }
  E e1 { 5 };		// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
  E e2 { -1 };		// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '-1' from 'int' to 'unsigned char'" "" { target c++17 } .-1 }
  E e3 { 5.0 };		// { dg-error "cannot convert 'double' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.0e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  E e4 { 5.2 };		// { dg-error "cannot convert 'double' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.\[0-9]*e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  B b2 = { 7 };		// { dg-error "invalid conversion from 'int' to 'B'" }
  C c2 = { C { 8 } };	// { dg-error "cannot convert 'int' to 'C' in initialization" "" { target c++14_down } }

  D *d7 = new D { 9 };	// { dg-error "cannot convert \[^\n\r]* to 'D' in initialization" "" { target c++14_down } }
  E *e5 = new E { -4 };	// { dg-error "cannot convert \[^\n\r]* to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '-4' from 'int' to 'unsigned char'" "" { target c++17 } .-1 }
  bar ({ 10 });		// { dg-error "cannot convert \[^\n\r]* to 'E'" }
  bar (E { 9 });	// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
  V v1 = { { 11 } };	// { dg-error "cannot convert '<brace-enclosed initializer list>' to 'E' in initialization" }
  V v2 = { E { 12 } };	// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
  V v3 = { E { 5.0 } };	// { dg-error "cannot convert 'double' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.0e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  V v4 = { 13 };	// { dg-error "cannot convert 'int' to 'E' in initialization" }
  if (B b3 { 5 })	// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
    ;
  if (B b4 { 4.0 })	// { dg-error "cannot convert 'double' to 'B' in initialization" "" { target c++14_down } }
    ;			// { dg-error "narrowing conversion of '4.0e.0' from 'double' to 'short int'" "" { target c++17 } .-1 }
  C c3 { 8L };		// { dg-error "cannot convert 'long int' to 'C' in initialization" "" { target c++14_down } }
  B b4 {short (c + 5)};	// { dg-error "invalid conversion from 'short int' to 'B'" "" { target c++14_down } }
  B b5 {c + 5};		// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
			// { dg-error "narrowing conversion of \[^\n\r]* from 'int' to 'short int'" "" { target { c++17 && { ! short_eq_int } } } .-1 }
  C c4 { ll };		// { dg-error "cannot convert 'long long int' to 'C' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of 'll' from 'long long int' to 'int'" "" { target c++17 } .-1 }
  C c5 {short (c + 5)};	// { dg-error "cannot convert 'short int' to 'C' in initialization" "" { target c++14_down } }
  C c6 {c + 5};		// { dg-error "cannot convert 'int' to 'C' in initialization" "" { target c++14_down } }
}

struct U
{
  U () : e { 5 } {}	// { dg-error "cannot convert \[^\n\r]* to 'E' in initialization" "" { target c++14_down } }
  U (int) : e { 5.0 } {}// { dg-error "cannot convert \[^\n\r]* to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.0e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  U (float) : e({ 6 }) {}// { dg-error "list-initializer for non-class type must not be parenthesized" }
			// { dg-error "cannot convert \[^\n\r]* to 'E' in initialization" "" { target *-*-* } .-1 }
  E e;
};

struct W
{
  A a { 5 };		// { dg-error "invalid conversion from 'int' to 'A'" }
  B b { 6 };		// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
  C c { 3.0f };		// { dg-error "cannot convert \[^\n\r]* to 'C' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '3.0e.0f' from 'float' to 'int'" "" { target c++17 } .-1 }
  D d = { 7 };		// { dg-error "cannot convert \[^\n\r]* to 'D' in initialization" }
};

template <int N>
void
foo2 ()
{
  A a1 { 5 };		// { dg-error "invalid conversion from 'int' to 'A'" }
  B b1 { 7 };		// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
  C c1 { s };
  D d1 { D(t) };	// { dg-error "invalid cast from type 'T' to type 'D'" }
  D d2 { t };		// { dg-error "cannot convert 'T' to 'D' in initialization" "" { target c++14_down } }
			// { dg-error "invalid cast from type 'T' to type 'D'" "" { target c++17 } .-1 }
  D d3 { 9 };		// { dg-error "cannot convert 'int' to 'D' in initialization" "" { target c++14_down } }
  D d4 { l };		// { dg-error "cannot convert 'long int' to 'D' in initialization" "" { target c++14_down } }
  D d5 { D(l) };
  D d6 { G };		// { dg-error "cannot convert 'A' to 'D' in initialization" "" { target c++14_down } }
  E e1 { 5 };		// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
  E e2 { -1 };		// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '-1' from 'int' to 'unsigned char'" "" { target c++17 } .-1 }
  E e3 { 5.0 };		// { dg-error "cannot convert 'double' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.0e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  E e4 { 5.2 };		// { dg-error "cannot convert 'double' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.\[0-9]*e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  B b2 = { 7 };		// { dg-error "invalid conversion from 'int' to 'B'" }
  C c2 = { C { 8 } };	// { dg-error "cannot convert 'int' to 'C' in initialization" "" { target c++14_down } }
  D *d7 = new D { 9 };	// { dg-error "cannot convert \[^\n\r]* to 'D' in initialization" "" { target c++14_down } }
  E *e5 = new E { -4 };	// { dg-error "cannot convert \[^\n\r]* to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '-4' from 'int' to 'unsigned char'" "" { target c++17 } .-1 }
  bar ({ 10 });		// { dg-error "cannot convert \[^\n\r]* to 'E'" }
  bar (E { 9 });	// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
  V v1 = { { 11 } };	// { dg-error "cannot convert '<brace-enclosed initializer list>' to 'E' in initialization" }
  V v2 = { E { 12 } };	// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
  V v3 = { E { 5.0 } };	// { dg-error "cannot convert 'double' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.0e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  V v4 = { 13 };	// { dg-error "cannot convert 'int' to 'E' in initialization" }
  if (B b3 { 5 })	// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
    ;
  if (B b4 { 4.0 })	// { dg-error "cannot convert 'double' to 'B' in initialization" "" { target c++14_down } }
    ;			// { dg-error "narrowing conversion of '4.0e.0' from 'double' to 'short int'" "" { target c++17 } .-1 }
  C c3 { 8L };		// { dg-error "cannot convert 'long int' to 'C' in initialization" "" { target c++14_down } }
  B b4 {short (c + 5)};	// { dg-error "invalid conversion from 'short int' to 'B'" "" { target c++14_down } }
  B b5 {c + 5};		// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
			// { dg-error "narrowing conversion of \[^\n\r]* from 'int' to 'short int'" "" { target { c++17 && { ! short_eq_int } } } .-1 }
  C c4 { ll };		// { dg-error "cannot convert 'long long int' to 'C' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of 'll' from 'long long int' to 'int'" "" { target c++17 } .-1 }
  C c5 {short (c + 5)};	// { dg-error "cannot convert 'short int' to 'C' in initialization" "" { target c++14_down } }
  C c6 {c + 5};		// { dg-error "cannot convert 'int' to 'C' in initialization" "" { target c++14_down } }
}

template <int N>
struct U2
{
  U2 () : e { 5 } {}	// { dg-error "cannot convert \[^\n\r]* to 'E' in initialization" "" { target c++14_down } }
  U2 (int) : e { 5.0 } {}// { dg-error "cannot convert \[^\n\r]* to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.0e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  U2 (float) : e({ 6 }) {}
  E e;
};

template <int N>
struct W2
{
  A a { 5 };		// { dg-error "invalid conversion from 'int' to 'A'" "" { target *-*-* } }
  B b { 6 };		// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
  C c { 3.0f };		// { dg-error "cannot convert \[^\n\r]* to 'C' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '3.0e.0f' from 'float' to 'int'" "" { target c++17 } .-1 }
  D d = { 7 };		// { dg-error "cannot convert \[^\n\r]* to 'D' in initialization" "" { target *-*-* } }
};

template <typename H, typename I, typename J, typename K, typename L, typename M>
void
foo3 ()
{
  void bar3 (L);
  H a1 { 5 };		// { dg-error "invalid conversion from 'int' to 'A'" }
  I b1 { 7 };		// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
  J c1 { s };
  K d1 { K(t) };	// { dg-error "invalid cast from type 'T' to type 'D'" }
  K d2 { t };		// { dg-error "cannot convert 'T' to 'D' in initialization" "" { target c++14_down } }
			// { dg-error "invalid cast from type 'T' to type 'D'" "" { target c++17 } .-1 }
  K d3 { 9 };		// { dg-error "cannot convert 'int' to 'D' in initialization" "" { target c++14_down } }
  K d4 { l };		// { dg-error "cannot convert 'long int' to 'D' in initialization" "" { target c++14_down } }
  K d5 { K(l) };
  K d6 { G };		// { dg-error "cannot convert 'A' to 'D' in initialization" "" { target c++14_down } }
  L e1 { 5 };		// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
  L e2 { -1 };		// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '-1' from 'int' to 'unsigned char'" "" { target c++17 } .-1 }
  L e3 { 5.0 };		// { dg-error "cannot convert 'double' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.0e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  L e4 { 5.2 };		// { dg-error "cannot convert 'double' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.\[0-9]*e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  I b2 = { 7 };		// { dg-error "invalid conversion from 'int' to 'B'" }
  J c2 = { J { 8 } };	// { dg-error "cannot convert 'int' to 'C' in initialization" "" { target c++14_down } }
  K *d7 = new K { 9 };	// { dg-error "cannot convert \[^\n\r]* to 'D' in initialization" "" { target c++14_down } }
  L *e5 = new L { -4 };	// { dg-error "cannot convert \[^\n\r]* to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '-4' from 'int' to 'unsigned char'" "" { target c++17 } .-1 }
  bar3 ({ 10 });	// { dg-error "cannot convert \[^\n\r]* to 'E'" }
  bar3 (E { 9 });	// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
  M v1 = { { 11 } };	// { dg-error "cannot convert '<brace-enclosed initializer list>' to 'E' in initialization" }
  M v2 = { L { 12 } };	// { dg-error "cannot convert 'int' to 'E' in initialization" "" { target c++14_down } }
  M v3 = { L { 5.0 } };	// { dg-error "cannot convert 'double' to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.0e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  M v4 = { 13 };	// { dg-error "cannot convert 'int' to 'E' in initialization" }
  if (I b3 { 5 })	// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
    ;
  if (I b4 { 4.0 })	// { dg-error "cannot convert 'double' to 'B' in initialization" "" { target c++14_down } }
    ;			// { dg-error "narrowing conversion of '4.0e.0' from 'double' to 'short int'" "" { target c++17 } .-1 }
  J c3 { 8L };		// { dg-error "cannot convert 'long int' to 'C' in initialization" "" { target c++14_down } }
  I b4 {short (c + 5)};	// { dg-error "invalid conversion from 'short int' to 'B'" "" { target c++14_down } }
  I b5 {c + 5};		// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
			// { dg-error "narrowing conversion of \[^\n\r]* from 'int' to 'short int'" "" { target { c++17 && { ! short_eq_int } } } .-1 }
  J c4 { ll };		// { dg-error "cannot convert 'long long int' to 'C' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of 'll' from 'long long int' to 'int'" "" { target c++17 } .-1 }
  J c5 {short (c + 5)};	// { dg-error "cannot convert 'short int' to 'C' in initialization" "" { target c++14_down } }
  J c6 {c + 5};		// { dg-error "cannot convert 'int' to 'C' in initialization" "" { target c++14_down } }
}

template <typename L>
struct U3
{
  U3 () : e { 5 } {}	// { dg-error "cannot convert \[^\n\r]* to 'E' in initialization" "" { target c++14_down } }
  U3 (int) : e { 5.0 } {}// { dg-error "cannot convert \[^\n\r]* to 'E' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '5.0e.0' from 'double' to 'unsigned char'" "" { target c++17 } .-1 }
  U3 (float) : e({ 6 }) {}
  L e;
};

template <typename H, typename I, typename J, typename K>
struct W3
{
  H a { 5 };		// { dg-error "invalid conversion from 'int' to 'A'" "" { target *-*-* } }
  I b { 6 };		// { dg-error "invalid conversion from 'int' to 'B'" "" { target c++14_down } }
  J c { 3.0f };		// { dg-error "cannot convert \[^\n\r]* to 'C' in initialization" "" { target c++14_down } }
			// { dg-error "narrowing conversion of '3.0e.0f' from 'float' to 'int'" "" { target c++17 } .-1 }
  K d = { 7 };		// { dg-error "cannot convert \[^\n\r]* to 'D' in initialization" "" { target *-*-* } }
};

void
test ()
{
  foo2<0> ();
  U2<0> u20;
  U2<1> u21 (5);
  W2<0> w2;		// { dg-message "" }
  foo3<A, B, C, D, E, V> ();
  U3<E> u30;
  U3<E> u31 (5);
  W3<A, B, C, D> w3;	// { dg-message "" }
}
