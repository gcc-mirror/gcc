// PR c++/120224
// { dg-do compile { target c++11 } }

template<class> using void_t = void;

template<class T>
void f(void*); // #1

template<class T>
void f(void_t<typename T::type>*) { } // { dg-error "not a class" } defn of #1

template<class T>
void g(int, void*); // #2

template<class T>
void g(int, void_t<typename T::type>*) { } // { dg-error "not a class" } defn of #2

int main() {
  f<int>(0); // { dg-error "no match" }
  g<int>(0, 0); // { dg-error "no match" }
}
