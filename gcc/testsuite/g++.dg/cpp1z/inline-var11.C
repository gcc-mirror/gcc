// PR c++/110031
// { dg-do compile { target c++17 } }

template <typename T>
[[deprecated]]
inline constexpr bool t = true ;

template <bool a>
struct enableif;

template<>
struct enableif<true>
{
        using y = int;
};
template <bool a>
using enableif_t = typename enableif<a>::y;

template <typename T, enableif_t<t<T>> = 0>   // { dg-warning "deprecated" }
struct A {  A(T &&)  {  }};

template <typename T>
struct A<T> {
  A(T &&) = delete;
  A() = delete;
};

int main(void)
{
  A<double> a(5.3); // { dg-error "use of deleted function" }
  return 0;
}
