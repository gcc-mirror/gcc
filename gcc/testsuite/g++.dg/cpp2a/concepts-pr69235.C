// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template<typename T>
concept Boolean =
  requires(T t)
  {
    { t } -> bool; // { dg-error "return-type-requirement is not a type-constraint" }
  };


template<typename T>
concept C =
  requires (T t)
  {
    { t } -> Boolean;
  };


template<typename T>
struct X;

template<typename T>
  requires (! C<typename T::type>)
struct X<T>
{
  using type = int;
};

template<typename T>
  requires C<typename T::type>
struct X<T>
{
  using type = int;
};

struct S
{
  using type = char;
};

void f()
{
  X<S>::type x;
}

