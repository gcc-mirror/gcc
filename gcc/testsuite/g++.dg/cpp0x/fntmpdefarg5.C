// Only print template subst context when it isn't redundant.
// { dg-require-effective-target c++11 }
// { dg-prune-output "error" }

template<class T> struct A { typedef typename T::type type; };

template <class T, class U = typename A<T>::type>
void f(T);

template <class T, class U = typename T::type>
void g(T);

int main()
{
  f(1);				// { dg-message "required from here" }
  g(1);				// { dg-bogus "required from here" }
}
