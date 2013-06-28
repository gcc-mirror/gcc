// PR c++/57599
// { dg-do compile { target c++11 } }

struct A { };
struct B : public A { };

template<class, class>
struct is_same { static constexpr bool value = false; };

template<class T>
struct is_same<T, T> { static constexpr bool value = true; };

template<class T>
T val();

static_assert(is_same<decltype(dynamic_cast<A*>(val<B*>())),
	      A*>::value, "Ouch");
static_assert(is_same<decltype(dynamic_cast<A&>(val<B&>())),
	      A&>::value, "Ouch");
static_assert(is_same<decltype(dynamic_cast<const A*>(val<B*>())),
	      const A*>::value, "Ouch");
static_assert(is_same<decltype(dynamic_cast<const A&>(val<B&>())),
	      const A&>::value, "Ouch");
static_assert(is_same<decltype(dynamic_cast<volatile A*>(val<B*>())),
	      volatile A*>::value, "Ouch");
static_assert(is_same<decltype(dynamic_cast<volatile A&>(val<B&>())),
	      volatile A&>::value, "Ouch");
static_assert(is_same<decltype(dynamic_cast<const volatile A*>(val<B*>())),
	      const volatile A*>::value, "Ouch");
static_assert(is_same<decltype(dynamic_cast<const volatile A&>(val<B&>())),
	      const volatile A&>::value, "Ouch");
