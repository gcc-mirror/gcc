template <class T, class V>
class A {
public:
  A (T at, V av);
  void print () { }
protected:
  T t;
  V v;
};

template <class T, class V>
A<T, V>::A (T at, V av) {
  t = at;
  v = av;
}


template <class T, class V>
class B: public virtual A<T, V> {
public:
  B (T at, V av);
  void print () { }
};

template <class T, class V>
B<T, V>::B (T at, V av) : A<T, V> (at, av) { }	// gets bogus error - 

int main () {
  int i = 2;
  double x = 2;

  B<int, double> ab(i, x);
  ab.print();

  return 0;
}
