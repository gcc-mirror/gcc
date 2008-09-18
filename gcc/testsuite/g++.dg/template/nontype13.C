// PR c++/19004

template<typename T>
struct Dummy
{
  void evil()
  {
    this->template tester<true>();
  }
      
  template<bool B>
  void tester()
  {
    bar<evil>()(); // { dg-error "constant" }
  }
  template<bool B>
  struct bar
  {
    void operator()()
    { }
  };
};

int main()
{
  Dummy<int> d;
  d.tester<true> (); // { dg-message "instantiated" }
}

