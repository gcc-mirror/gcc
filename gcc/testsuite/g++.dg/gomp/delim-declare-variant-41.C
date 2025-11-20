/* { dg-do compile { target c++11 } } */

template<typename T, typename U>
class is_same {
 public:
  static constexpr bool value = false;
};

template<typename T>
class is_same<T, T> {
 public:
  static constexpr bool value = true;
};

template<typename T>
void fn (T&&) { }

#pragma omp begin declare variant match(implementation={vendor("gnu")})
template<typename T>
void fn(T&&) {
  static_assert(is_same<T, int>::value);
}
#pragma omp end declare variant

int main()
{
  int lvalue = 42;
  fn(0);
}
