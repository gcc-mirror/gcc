// PR c++/64085
// { dg-do compile { target c++14 } }

template<typename T>
struct reference_wrapper
{
  T& get() const noexcept;
};

template<class T>
auto make_monad(reference_wrapper<T> arg) {
  return [&captive = arg.get()](auto&&) { return 1; };
}

int main()
{
  make_monad(reference_wrapper<int&>());
}
