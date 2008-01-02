// PR c++/33463

namespace std
{
  class type_info {};
}

template<int> void foo()
{
  !typeid(void); // { dg-error "!typeid\\(void\\)|candidates" }
}
