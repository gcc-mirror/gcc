// PR c++/9050, DR 147, 318

struct Y
{
  template <typename T> Y (T);
  template <typename T> void foo (T);
};

template <>      Y::Y<int>   (int)  { }
