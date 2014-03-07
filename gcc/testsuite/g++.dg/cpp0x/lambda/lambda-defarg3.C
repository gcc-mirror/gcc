// PR c++/55223
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=0" }
// { dg-final { scan-assembler "_ZN8functionC1IZN1CIiE4testES_Ed_UliE_EET_" } }

struct function
{
  template <class U> function(U u) { }
};

template<typename T> struct C
{
  static T test(function f = [](int i){return i;}) { }
};

int main()
{
  C<int>::test();
}
