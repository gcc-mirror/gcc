// { dg-do run  }
class A {
public:
};

template <class T>
class B: public virtual A {
public:
  B ();
  ~B ();
};

template <class T>
B<T>::B () { }

template <class T>
B<T>::~B () { }

int main () {
  B<int> ab;

  return 0;
}
