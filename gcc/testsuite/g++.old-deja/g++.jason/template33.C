// Test nested enums in templates.
// Build don't link:

template <class T>
class A
{
public:
  enum muni {X, Y};
  
  muni e() { return X; };
  muni f();
};

template <class T>
A<T>::muni A<T>::f() { return X; }

template class A<int>;
