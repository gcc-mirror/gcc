// { dg-options "-std=gnu++11" }
void print_all() {}

template<typename T, typename... Rest>
void print_all(const T& t, const Rest&... rest)
{
  print_all(rest...);
}

void f()
{
  print_all();
  print_all(1);
  print_all(1, 3.14159);
  print_all("Hello, World!", 17, 3.14159);
}
