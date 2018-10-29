// PR c++/86480
// { dg-do compile { target c++17 } }

template <class...> constexpr bool val = true;

template <class... T>
void f()
{
  [](auto... p)
    {
      []{
	if constexpr (val<T..., decltype(p)...>) { return true; }
	return false;
      }();
    }(42);
}

int main()
{
  f<int>();
}
