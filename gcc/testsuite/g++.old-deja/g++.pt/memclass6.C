// Compiler: egcs-2.91.12 980302
// Error:    compiler error in ctor of 'foo::bar<T>::bar(T const &)'

struct foo
{
  template <typename T>
        struct bar
        {
          bar(T const &t) : tt(t) {}
          T tt;
        };
};

int main()
{
  foo::bar<int> fb(3);
  return 0;
}
