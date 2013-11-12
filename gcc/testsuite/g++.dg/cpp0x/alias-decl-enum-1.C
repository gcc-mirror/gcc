// PR c++/57734
// { dg-do compile { target c++11 } }

template<typename T, typename U>
struct same_type { static const bool value = false; };

template<typename T>
struct same_type<T, T> { static const bool value = true; };

enum e { zero };
enum class eclass { one };

template<typename T>
using enum_alias = e;

template<typename T>
using eclass_alias = eclass;

typedef enum_alias<void> etest0;
typedef enum_alias<void> etest0;
typedef enum_alias<int>  etest0;
typedef enum_alias<int>  etest1;

static_assert (same_type<etest0, etest1>::value, "");

typedef eclass_alias<void> ectest0;
typedef eclass_alias<void> ectest0;
typedef eclass_alias<int>  ectest0;
typedef eclass_alias<int>  ectest1;

static_assert (same_type<ectest0, ectest1>::value, "");

template<typename T>
enum_alias<T> efoo(T f) { return enum_alias<T>::zero; }

template<typename T>
constexpr enum_alias<T> cefoo(T f) { return enum_alias<T>::zero; }

static_assert ( cefoo(1) == e::zero, "");

template<typename T>
eclass_alias<T> ecfoo(T f) { return eclass_alias<T>::one; }

template<typename T>
constexpr eclass_alias<T> cecfoo(T f) { return eclass_alias<T>::one; }

static_assert ( cecfoo(1) == eclass::one, "");
