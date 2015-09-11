// PR c++/64227
// { dg-do compile { target c++14 } }

template<typename T>
struct remove_reference
{ typedef T   type; };

template<typename T>
struct remove_reference<T&>
{ typedef T   type; };

template<typename T>
struct remove_reference<T&&>
{ typedef T   type; };

template<typename T>
constexpr T&&
forward(typename remove_reference<T>::type& t) noexcept
{ return static_cast<T&&>(t); }

template<typename T>
constexpr T&&
forward(typename remove_reference<T>::type&& t) noexcept
{ return static_cast<T&&>(t); }

template<typename T>
auto greater_than(T&& t)
{
  return [val = forward<T&&>(t)] (const auto& arg) { return arg > val; };
}

template<typename Functor>
void g(Functor f)
{
  for (int i = 0; i < 100000; i++)
    f(i);
}

int main()
{
  g(greater_than(10));
}
