// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
class S {
public:
  template <class U>
  class C {
  public:
    void f() { S::i = 3; }
  };

  template <class U>
  friend class C;

private:
  static int i;
};


template void S<int>::C<double>::f();
