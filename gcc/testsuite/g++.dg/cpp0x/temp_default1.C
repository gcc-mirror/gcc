// { dg-do compile { target c++11 } }

template<typename T, typename U>
struct is_same
{
  static const bool value = false;
};

template<typename T>
struct is_same<T, T> {
  static const bool value = true;
};

template<typename T = int> void f()
{
  static_assert(is_same<T, int>::value, 
                "T can only be instantiated with an int");
}

template<typename T = int, typename U>
void f(U)
{
  static_assert(is_same<T, int>::value, 
                "T can only be instantiated with an int");
}

void g()
{
  float pi = 3.14159;
  f();
  f(pi);
}
