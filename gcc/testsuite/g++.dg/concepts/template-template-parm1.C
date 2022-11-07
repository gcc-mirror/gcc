// PR c++/66937
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

#include <tuple>

namespace detail
{
  template<typename T, template<typename...> class Sink>
  struct copy_tuple_args_impl;

  template<typename... Args, template<typename...> class Sink>
  struct copy_tuple_args_impl<std::tuple<Args...>, Sink>
  {
    using type = Sink<Args...>;
  };
}

// copy_tuple_args copies the template arguments of a tuple into another template
// copy_tuple_args does not care about constraints whatsoever.
template<typename Tuple, template<typename...> class Sink>
using copy_tuple_args = typename detail::copy_tuple_args_impl<Tuple, Sink>::type;

// A concept of a column
template <typename T>
concept bool Column()
{
  return requires()
    {
      typename T::_name_t;
    };
}

// column_list is constrained to Column arguments
template<Column... C>
struct column_list
{
};

// Here are some columns
struct A
{
  using _name_t = int;
};

struct B
{
  using _name_t = int;
};


int main()
{
  using ColumnTuple = std::tuple<A, B>;
  using ColumnList = copy_tuple_args<ColumnTuple, column_list>; // This fails, but should not
}

