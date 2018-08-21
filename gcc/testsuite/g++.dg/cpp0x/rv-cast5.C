// { dg-do compile { target c++11 } }

template <typename T>
struct hold {
  T value;
  constexpr T&& operator()() && { return static_cast<T&&>(value); }
};

int main()
{
  hold<bool&&>{static_cast<bool>(42)}();
}
