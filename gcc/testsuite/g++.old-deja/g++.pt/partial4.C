// Build don't link:
// Origin: Gabriel Dos Reis <Gabriel.Dos-Reis@cmla.ens-cachan.fr>

// Bug 29.  We failed to verify that template argument deduction
// produces a valid result in nondeduce context.

template<class T> struct Y { typedef T X; };

template<class T, class U> struct Base {};

template<class T> struct Base<T, typename T::X> {};

template<class T> struct Base<typename T::X, T> {};

template<class T, class U> struct Derived : Base <T, U> {};

struct A {};

template<class T> struct Derived<A, T> : Base< Y<T>, Y<T> > {};

int main()
{
  Derived<A, int> d;
}
