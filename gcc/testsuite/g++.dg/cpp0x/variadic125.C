// PR c++/52380
// { dg-do compile { target c++11 } }

template<typename T>
struct S
{
  template<typename U>
  struct Unary                        // Line 5
  {};

  template<unsigned, typename... Args>
  struct Dispatch                     // Line 9
    : public Unary<Args...>
  {};

  template<typename... Args>
  struct Variadic
    : public Dispatch<sizeof...(Args), Args...>
  {};
};

int main()
{
  S<void>::Variadic<void> z;
}
