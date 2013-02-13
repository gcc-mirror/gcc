// PR c++/55710
// { dg-do link { target c++11 } }

template <class T>
struct X {
  static void (*code) ();
};

template <class T>
void (*X<T>::code) () = []{};  // Line 7

struct Y {
  void (*code) () = []{} ; // Line 10
  void operator()() { code(); }
};

int main () {
  X<int>::code();
  Y()();
}
