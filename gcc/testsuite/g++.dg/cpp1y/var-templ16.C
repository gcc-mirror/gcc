// { dg-do compile { target c++14 } }

template <class T> T t = 42;
template <class T> T* t<T*> = nullptr;

void *p = t<void*>;
