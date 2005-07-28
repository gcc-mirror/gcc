/* Test visibility attribute on template member function
   instantiations. */

/* { dg-do compile } */
/* { dg-options "-fvisibility=hidden" } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-not-hidden "_ZN7myClassIiE3maxEii" } } */

#define EXPORT __attribute__((visibility("default")))

template <class T>
class EXPORT myClass {
public:
  T max (T t1, T t2);
};

template <class T>
T myClass<T>::max (T t1, T t2) {
  return (t1 > t2 ? t1 : t2);
}

template class myClass<int>;
