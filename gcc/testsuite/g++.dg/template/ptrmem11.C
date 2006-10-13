// PR c++/18407

template <typename Klasse>
struct the_base{
  template <void (Klasse::*Fn)()> void foo() { }
};

template <typename T>
struct derivedT: the_base<derivedT<T> > {
  typedef the_base<derivedT<T> > parent;
  void ice(){
    this->parent::template foo< &derivedT<T>::ice>();
  }
};

int main() {
  derivedT<int> dT;
  dT.ice();
}
