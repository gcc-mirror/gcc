// PR c++/85149
// { dg-do run { target c++17 } }

template <typename T> struct is_void { static constexpr bool value = false; };
template <> struct is_void<void> { static constexpr bool value = true; };

template<typename S, typename T>
constexpr decltype(auto) pipeline(S source, T target)
{
  return [=](auto... args)
    {
      if constexpr(false
		   && is_void<decltype(source(args...))>::value)
	{
	  source(args...);
	  return target();
	}
      else
	{
	  return target(source(args...));
        }
    };
}

int main() {
  int i = 10;
  int j = 42;
  auto p = pipeline([&]{ return j; },
		    [=](int val){ return val * i; });
  if (p() != 420)
    __builtin_abort();
}
