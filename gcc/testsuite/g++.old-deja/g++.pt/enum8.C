// { dg-do assemble  }

template <int I>
void f();

template <>
void f<4>() {}

template <class T>
struct S
{
  enum E { a = 1, b = a + 3 };
};

int main()
{
  f<S<int>::b>();
}
