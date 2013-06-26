// PR c++/53211
// { dg-do compile { target c++11 } }

template<typename A, typename B>
  struct is_same { static const bool value = false; };

template<typename A>
  struct is_same<A, A> { static const bool value = true; };

template<typename... Args>
void func(Args... args)
{
  int arr[] = { args... };
  static_assert (is_same<decltype(arr), int[sizeof...(Args)]>::value, "");
}

int main()
{
  func(1, 2, 3, 4);
}
