// { dg-do assemble  }
// Test nested enums in templates.

template <class T>
class A
{
public:
  enum muni {X, Y};
  
  muni e() { return X; }
  muni f();
};

template <class T>
typename A<T>::muni A<T>::f() { return X; }

template class A<int>;
