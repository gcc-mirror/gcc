// { dg-do compile { target c++20 } }

template <class T>
struct A
{
  friend bool operator==(const A&, const A&) requires true = default;
};

int main()
{
  A<int>() == A<int>();
}
