// { dg-do run  }
// Bug: g++ forgets about the instantiation of class1 when looking up
// class11_value, and tries to look things up in class1<class2>.

template<class ItIsInt>
struct class1 {
  struct class11 {
    typedef ItIsInt class11_value;
  };
};

template<class ItIsClass2>
struct class3 {
  int f();
};

template<class ItIsClass2>
int class3<ItIsClass2>::f()
{
  return typename class1<typename ItIsClass2::class2_value>::class11::class11_value(10);
}

struct class2 {
  typedef int class2_value;
};

int main()
{
  class3<class2> the_class3;
  the_class3.f();
}
