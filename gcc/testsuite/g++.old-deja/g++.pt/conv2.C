// Build don't link:

template<class T>
class A {
public:
  operator const T*() const;
  const T* cast() const;
};

template<class T>
const T* A<T>::cast() const {
  return operator const T*();
}

template class A<char>;
