// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

template<typename T>
concept bool Boolean()
{
  return requires(T t)
  {
    { t } -> bool;
  };
}

template<typename T>
concept bool C()
{
  return requires (T t)
  {
    { t } -> Boolean;
  };
}

template<typename T>
struct X;

template<typename T>
  requires ! C<typename T::type>()
struct X<T>
{
  using type = int;
};

template<typename T>
  requires C<typename T::type>()
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

