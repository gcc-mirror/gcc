template <typename T>
class MyClass
{
public:
  __attribute__ ((__always_inline__)) inline MyClass () { ; }
};

extern template class MyClass<double>;

void Func()
{
  MyClass<double> x;
}
