// Origin: dgregor@gcc.gnu.org
// PR c++/11384
// foo<int>::_S_something was not being emitted (as a weak definition).

// { dg-do run }
// { dg-require-weak "" }

template<typename T> 
  struct foo
  {
    static const T _S_something;
  };

template<typename T>
  const T foo<T>::_S_something = T();

int main()
{
  const int* p = &foo<int>::_S_something;
  return 0;
}
