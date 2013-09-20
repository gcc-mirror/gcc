// PR c++/58435
// { dg-do compile { target c++11 } }

template<typename T, typename U>
struct same { static const bool value = false; };
template<typename T>
struct same<T, T> { static const bool value = true; };

template <template <typename> class F, typename T> struct apply
{ typedef F<T> type; };
template <template <typename> class F, typename T> struct applyc
{ typedef const F<T> type; };
template <template <typename> class F, typename T> struct applyv
{ typedef volatile F<T> type; };
template <template <typename> class F, typename T> struct applycv
{ typedef const volatile F<T> type; };

template <typename T> using map = T;
template <typename T> using mapc = const T;
template <typename T> using mapv = volatile T;
template <typename T> using mapcv = const volatile T;

static_assert(same<apply<map, int>::type, int>::value, "");
static_assert(same<apply<mapc, int>::type, const int>::value, "");
static_assert(same<apply<mapv, int>::type, volatile int>::value, "");
static_assert(same<apply<mapcv, int>::type, const volatile int>::value, "");

static_assert(same<applyc<map, int>::type, const int>::value, "");
static_assert(same<applyc<mapc, int>::type, const int>::value, "");
static_assert(same<applyc<mapv, int>::type, const volatile int>::value, "");
static_assert(same<applyc<mapcv, int>::type, const volatile int>::value, "");

static_assert(same<applyv<map, int>::type, volatile int>::value, "");
static_assert(same<applyv<mapc, int>::type, const volatile int>::value, "");
static_assert(same<applyv<mapv, int>::type, volatile int>::value, "");
static_assert(same<applyv<mapcv, int>::type, const volatile int>::value, "");

static_assert(same<applycv<map, int>::type, const volatile int>::value, "");
static_assert(same<applycv<mapc, int>::type, const volatile int>::value, "");
static_assert(same<applycv<mapv, int>::type, const volatile int>::value, "");
static_assert(same<applycv<mapcv, int>::type, const volatile int>::value, "");
