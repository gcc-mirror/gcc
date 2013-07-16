// PR c++/52688
// { dg-do link }

template<class T>
struct A {
  static bool test() {
    static bool value = false;
    if (value)
      return false;
    struct S {
      S() { value = true; }
    };
    static S s;
    return true;
  }
};

int main()
{
  A<int>::test();
}
