/* { dg-do compile { target c++11 } } */

/* This test case fails in omp_finish_variant_function because base_decl
   is a SCOPE_REF and it cannot be resolved to an actual base function decl
   to hang the variant attribute on.  */

template<typename T, typename U>
  class is_same {
  static constexpr bool value = false;
};

template<typename T>
class is_same<T, T> {
  static constexpr bool value = true;
};

template<typename T>
void fn (T&&) { }

#pragma omp begin declare variant match(implementation={vendor("gnu")})
template<typename T>
void fn(T&&) {  // { dg-bogus "base function cannot be resolved" "" { xfail *-*-* } }
  static_assert(is_same<T, int>::value);
}
#pragma omp end declare variant

int main()
{
  int lvalue = 42;
  fn(0);
}
