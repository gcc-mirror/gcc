template <class T>
struct A
{
  friend void bar(A<T> a) {}
};

void bar(A<int>);

int main()
{
  A<int> a;

  bar(a);
}

