// PR c++/50523

template <class T>
struct A
{
  A(const T&);
  operator T&() const;
  operator const T&() const;
};

int main()
{
  A<int> a(1);
  A<int> a2(a);
}
