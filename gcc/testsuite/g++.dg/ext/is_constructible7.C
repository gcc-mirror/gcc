// PR c++/96645
// { dg-do compile { target c++11 } }

template<bool B>
struct bool_constant
{
  static constexpr bool value = B;
  using type = bool_constant;
};

using true_type = bool_constant<true>;

template<typename T>
struct is_default_constructible
  : bool_constant<__is_constructible(T)> // { dg-error "default member init" }
{ };

void testVarStruct()
{
  struct DataWithStruct {
    struct A {
      int number = 5; // compiles, if remove initialization
    };

    // { dg-prune-output "could not convert" }
    is_default_constructible<A>::type t = true_type{};
  };
}
