// { dg-do compile { target c++11 } }

template<class T, T N, class... Ts, class = __type_pack_element<N, Ts...>>
constexpr int f(int) { return 1; }

template<class T, T N, class... Ts>
constexpr int f(...) { return 2; };

int p;

static_assert(f<int, 0, void, char>(0) == 1, "");
static_assert(f<int, 1, void, char>(0) == 1, "");
static_assert(f<int, 2, void, char>(0) == 2, "");
static_assert(f<int*, &p, void, char>(0) == 2, "");

template<class T, class U> struct A;
template<class T> struct A<T, __type_pack_element<sizeof(T), void, long>> { };
template struct A<char, long>;

template<class T, class U> struct B;
template<class T> struct B<T, __type_pack_element<0, T, short>> { };
template struct B<int, int>;
