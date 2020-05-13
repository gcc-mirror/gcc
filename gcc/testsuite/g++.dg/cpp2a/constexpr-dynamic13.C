// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++20 } }

// Adopted from g++.old-deja/g++.other/dyncast1.C.

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
static_assert (dynamic_cast<D*> ((A*)&d) == nullptr);
static_assert (dynamic_cast<D*> ((B*)&d) == nullptr);
static_assert (&d == dynamic_cast<D*> ((C*)&d));
static_assert (dynamic_cast<C*> ((B*)&d) == nullptr);

constexpr DD dd;
static_assert (dynamic_cast<DD*> ((A*)&dd) == nullptr);
static_assert (dynamic_cast<DD*> ((B*)&dd) == nullptr);

constexpr DDD ddd;
static_assert (dynamic_cast<DDD*> ((A*)&ddd) == nullptr);
static_assert (dynamic_cast<DDD*> ((B*)&ddd) == nullptr);
static_assert (dynamic_cast<CCC*> ((B*)&ddd) == nullptr);

// 1.2. multiple inheritance case
// 1.2.1. all bases are public
 
struct E : D, CC {};
struct EE : CC, D {}; //Will search in reverse order.

constexpr E e;
static_assert (dynamic_cast<E*> ((A*)(D*)&e) == nullptr);
static_assert (dynamic_cast<E*> ((B*)(D*)&e) == nullptr);
static_assert (&e == dynamic_cast<E*> ((C*)(D*)&e));
static_assert (&e == dynamic_cast<E*> ((B*)(CC*)&e));
static_assert ((CC*)&e == dynamic_cast<CC*> ((B*)(CC*)&e));

constexpr EE ee;
static_assert (dynamic_cast<EE*> ((A*)(D*)&ee) == nullptr);
static_assert (dynamic_cast<EE*> ((B*)(D*)&ee) == nullptr);
static_assert (&ee == dynamic_cast<EE*> ((C*)(D*)&ee));
static_assert (&ee == dynamic_cast<EE*> ((B*)(CC*)&ee));
static_assert ((CC*)&ee == dynamic_cast<CC*> ((B*)(CC*)&ee));

// 1.2.2 one or more branches are nonpublic

struct X : private BB, E {};
struct Y : AA, private B {};

class XX : BB, E {};

constexpr X x;
static_assert (&x == dynamic_cast<X*>((B*)(CC*)(E*)&x));

constexpr XX xx;
static_assert (dynamic_cast<XX*>((B*)(CC*)(E*)&xx) == nullptr);	

constexpr Y y;
static_assert (dynamic_cast<Y*>((B*)&y) == nullptr);
static_assert (dynamic_cast<Y*>((A*)(B*)&y) == nullptr);

// 2. crosscast

struct J { virtual void j(); };
struct K : CC, private J {}; 
class KK : J, CC{};
		
static_assert (dynamic_cast<CC*> ((B*)(D*)&e) == nullptr);
static_assert ((CC*)&e == dynamic_cast<CC*> ((C*)(D*)&e));

constexpr K k;
static_assert (dynamic_cast<J*> ((B*)&k) == nullptr);
constexpr KK kk;
static_assert (dynamic_cast<J*> ((CC*)&kk) == nullptr);
