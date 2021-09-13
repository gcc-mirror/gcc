// PR c++/98611
// { dg-do compile { target c++20 } }

template <class T, class U>
concept IsSame = __is_same(T, U);

template <class T, template <class...> class _Class>
concept IsInstantiationOf = requires(T object) {
 { _Class{object} } -> IsSame<T>;
};

template <class T> struct Degrees {};
static_assert(IsInstantiationOf<Degrees<int>, Degrees>);

template <class T> struct NotDegrees {};
static_assert(!IsInstantiationOf<Degrees<int>, NotDegrees>);
