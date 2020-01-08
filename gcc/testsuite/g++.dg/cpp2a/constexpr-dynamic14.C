// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++2a } }

// Adopted from g++.old-deja/g++.other/dyncast1.C.
// But use reference dynamic_cast.

// 1. downcast
// 1.1. single inheritance case

struct A { virtual void a(); };
struct AA : A {};
struct B : A {};
struct BB : B {};
class C : B {};
struct D : C {};

struct CC : B {};
class DD : CC {};

class CCC : protected B {};
class DDD : protected CCC {};

constexpr D d;
constexpr bool b01 = (dynamic_cast<D&> ((A&)d), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const A. of its operand is a non-public base class of dynamic type .D." "" { target *-*-* } .-1 }
constexpr bool b02 = (dynamic_cast<D&> ((B&)d), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const B. of its operand is a non-public base class of dynamic type .D." "" { target *-*-* } .-1 }
static_assert (&d == &dynamic_cast<const D&> ((C&)d));
constexpr bool b03 = (dynamic_cast<C&> ((B&)d), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const B. of its operand is a non-public base class of dynamic type .D." "" { target *-*-* } .-1 }

constexpr DD dd;
constexpr bool b04 = (dynamic_cast<DD&> ((A&)dd), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const A. of its operand is a non-public base class of dynamic type .DD." "" { target *-*-* } .-1 }
constexpr bool b05 = (dynamic_cast<DD&> ((B&)dd), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const B. of its operand is a non-public base class of dynamic type .DD." "" { target *-*-* } .-1 }

constexpr DDD ddd;
constexpr bool b06 = (dynamic_cast<DDD&> ((A&)ddd), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const A. of its operand is a non-public base class of dynamic type .DDD." "" { target *-*-* } .-1 }
constexpr bool b07 = (dynamic_cast<DDD&> ((B&)ddd), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const B. of its operand is a non-public base class of dynamic type .DDD." "" { target *-*-* } .-1 }
constexpr bool b08 = (dynamic_cast<CCC&> ((B&)ddd), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const B. of its operand is a non-public base class of dynamic type .DDD." "" { target *-*-* } .-1 }

// 1.2. multiple inheritance case
// 1.2.1. all bases are public
 
struct E : D, CC {};
struct EE : CC, D {}; //Will search in reverse order.

constexpr E e;
constexpr bool b09 = (dynamic_cast<E&> ((A&)(D&)e), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .A. of its operand is a non-public base class of dynamic type .E." "" { target *-*-* } .-1 }
constexpr bool b10 = (dynamic_cast<E&> ((B&)(D&)e), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .B. of its operand is a non-public base class of dynamic type .E." "" { target *-*-* } .-1 }
static_assert (&e == &dynamic_cast<E&> ((C&)(D&)e));
static_assert (&e == &dynamic_cast<E&> ((B&)(CC&)e));
static_assert (&(CC&)e == &dynamic_cast<CC&> ((B&)(CC&)e));

constexpr EE ee;
constexpr bool b11 = (dynamic_cast<EE&> ((A&)(D&)ee), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .A. of its operand is a non-public base class of dynamic type .EE." "" { target *-*-* } .-1 }
constexpr bool b12 = (dynamic_cast<EE&> ((B&)(D&)ee), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .B. of its operand is a non-public base class of dynamic type .EE." "" { target *-*-* } .-1 }
static_assert (&ee == &dynamic_cast<EE&> ((C&)(D&)ee));
static_assert (&ee == &dynamic_cast<EE&> ((B&)(CC&)ee));
static_assert (&(CC&)ee == &dynamic_cast<CC&> ((B&)(CC&)ee));

// 1.2.2 one or more branches are nonpublic

struct X : private BB, E {};
struct Y : AA, private B {};

class XX : BB, E {};

constexpr X x;
static_assert (&x == &dynamic_cast<X&>((B&)(CC&)(E&)x));

constexpr XX xx;
constexpr bool b13 = (dynamic_cast<XX&>((B&)(CC&)(E&)xx), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .B. of its operand is a non-public base class of dynamic type .XX." "" { target *-*-* } .-1 }

constexpr Y y;
constexpr bool b14 = (dynamic_cast<Y&>((B&)y), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const B. of its operand is a non-public base class of dynamic type .Y." "" { target *-*-* } .-1 }
constexpr bool b15 = (dynamic_cast<Y&>((A&)(B&)y), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .A. of its operand is a non-public base class of dynamic type .Y." "" { target *-*-* } .-1 }

// 2. crosscast

struct J { virtual void j(); };
struct K : CC, private J {}; 
class KK : J, CC{};
		
constexpr bool b16 = (dynamic_cast<CC&> ((B&)(D&)e), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .B. of its operand is a non-public base class of dynamic type .CC." "" { target *-*-* } .-1 }
static_assert (&(CC&)e == &dynamic_cast<CC&> ((C&)(D&)e));

constexpr K k;
constexpr bool b17 = (dynamic_cast<J&> ((B&)k), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "dynamic type .K. of its operand does not have an unambiguous public base class .J." "" { target *-*-* } .-1 }
constexpr KK kk;
constexpr bool b18 = (dynamic_cast<J&> ((CC&)kk), true); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const CC. of its operand is a non-public base class of dynamic type .KK." "" { target *-*-* } .-1 }
