// PR c++/124229
// { dg-do compile { target c++26 } }

template<typename T>
void foo () { }

int main()
{
  template for (constexpr auto val : { 42 })
    {
      using U = decltype(val);
      foo<U>();
    }
}
