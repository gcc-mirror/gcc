// The from_range_t default ctor should not break the PR99599 workaround
// { dg-do compile { target c++20 } }

template<typename T>
struct S { T t; };

template<typename T>
concept C = sizeof(S<T>) > 0;

struct I;

struct from_range_t {
    explicit from_range_t() = default;
};
inline constexpr from_range_t from_range;

template<typename T>
concept FromRange = __is_same_as (T, from_range_t);

//#define WORKAROUND
#ifdef WORKAROUND
template<FromRange U, C T>
void f(U, T*);
#else
template<C T>
void f(from_range_t, T*);
#endif

void f(...);

void g(I* p)
{
    ::f(0, p);
}
