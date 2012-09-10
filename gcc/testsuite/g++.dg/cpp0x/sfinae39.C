// PR c++/54191
// { dg-do compile { target c++11 } }

struct B
{};

struct D
  : private B
{};

template<typename T>
T &&declval();


template<typename From, typename = decltype(B{declval<From>()})>
constexpr bool test_braced_cast_to_base(int)
{ return true; }

template<typename>
constexpr bool test_braced_cast_to_base(bool)
{ return false; }

static_assert(!test_braced_cast_to_base<D>(0), "");


template<typename From, typename = decltype(D{declval<From>()})>
constexpr bool test_braced_cast_to_derived(int)
{ return true; }

template<typename>
constexpr bool test_braced_cast_to_derived(bool)
{ return false; }

static_assert(!test_braced_cast_to_derived<B>(0), "");


typedef B *PB;

template<typename From, typename = decltype(PB{declval<From>()})>
constexpr bool test_braced_cast_to_ptr_to_base(int)
{ return true; }

template<typename>
constexpr bool test_braced_cast_to_ptr_to_base(bool)
{ return false; }

static_assert(!test_braced_cast_to_ptr_to_base<D *>(0), "");


typedef D *PD;

template<typename From, typename = decltype(PD{declval<From>()})>
constexpr bool test_braced_cast_to_ptr_to_derived(int)
{ return true; }

template<typename>
constexpr bool test_braced_cast_to_ptr_to_derived(bool)
{ return false; }

static_assert(!test_braced_cast_to_ptr_to_derived<B *>(0), "");


template<typename From, typename To,
         typename = decltype(static_cast<To>(declval<From>()))>
constexpr bool test_static_cast(int)
{ return true; }

template<typename, typename>
constexpr bool test_static_cast(bool)
{ return false; }

static_assert(!test_static_cast<B &, D &>(0), "");
static_assert(!test_static_cast<B *, D *>(0), "");


template<typename From, typename To,
         typename = decltype(dynamic_cast<To>(declval<From>()))>
constexpr bool test_dynamic_cast(int)
{ return true; }

template<typename, typename>
constexpr bool test_dynamic_cast(bool)
{ return false; }

static_assert(!test_dynamic_cast<D &, B &>(0), "");
static_assert(!test_dynamic_cast<D *, B *>(0), "");


int B::*pm = 0;

template<typename T, typename = decltype(declval<T>().*pm)>
constexpr bool test_member_ptr_dot(int)
{ return true; }

template<typename>
constexpr bool test_member_ptr_dot(bool)
{ return false; }

static_assert(!test_member_ptr_dot<D>(0), "");


template<typename T, typename = decltype(declval<T>()->*pm)>
constexpr bool test_member_ptr_arrow(int)
{ return true; }

template<typename>
constexpr bool test_member_ptr_arrow(bool)
{ return false; }

static_assert(!test_member_ptr_arrow<D *>(0), "");


template<typename T, typename U,
         typename = decltype(declval<T>() < declval<U>())>
constexpr bool test_rel_op(int)
{ return true; }

template<typename, typename>
constexpr bool test_rel_op(bool)
{ return false; }

static_assert(!test_rel_op<D *, B *>(0), "");


template<typename T, typename U,
         typename = decltype(declval<T>() == declval<U>())>
constexpr bool test_eq(int)
{ return true; }

template<typename, typename>
constexpr bool test_eq(bool)
{ return false; }

static_assert(!test_eq<D *, B *>(0), "");


template<typename T, typename U,
         typename = decltype(false ? declval<T>() : declval<U>())>
constexpr bool test_cond_op(int)
{ return true; }

template<typename, typename>
constexpr bool test_cond_op(bool)
{ return false; }

static_assert(!test_cond_op<B *, D *>(0), "");
