// { dg-do run  }
struct S 
{
  template <class T>
  void f(T (&i)[7])
    {}

  void g()
    {
      int i[] = {1, 2, 3, 4, 5, 6, 7};
      f(i);
      int j[7];
      f(j);
    }
};

struct foo {
  template <typename T, int N>
  static T* array_end(T(&array)[N]) { return &array[N]; }
};

struct X
{
  template <class T1>
  void f(const T1&) {}
};

int main(int ac, char* av[]) {
  S s;
  s.g();
  int i[] = {1,2,3,4,5};
  int* e = foo::array_end(i);
  X x;
  x.f("hello");
}
 
