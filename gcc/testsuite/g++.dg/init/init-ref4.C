// Origin: dgregor@gcc.gnu.org
// PR c++/11384
// foo<int>::_S_something was not being emitted.

// { dg-do run { xfail *-*-aout *-*-coff *-*-hpux* *-*-hms } }
// On targets that don't support weak symbols, we require an explicit

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

