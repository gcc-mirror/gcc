// PR c++/53464
// { dg-do compile { target c++11 } }

template <int value>
struct bar
{
  static constexpr int get()
  {
    return value;
  }
};

template <typename A, int value = A::get()>
struct foo
{
};

int main()
{
  typedef foo<bar<0>> type;
  return 0;
}
