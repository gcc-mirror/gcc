// { dg-do compile { target c++14 } }

template <class T> T t1 = 42;
template <class T> T* t1<T> = nullptr; // { dg-error "partial" }

template <class T> T t2 = 42;
template <class T> T* t2<T*> = nullptr;
template <class T> T* t2<T*> = nullptr; // { dg-error "redefinition" }

template <class T, class U> T t3 = U();
template <class T> T t3<T,int> = 42;
template <class U> int t3<int,U> = U();

int i = t3<int,int>;		// { dg-error "ambiguous" }

template <class T> T t4 = T();
void *p = t4<void*>;
template <class T> T* t4<T*> = nullptr; // { dg-error "after instantiation" }
