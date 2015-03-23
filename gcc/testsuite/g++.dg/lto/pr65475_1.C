namespace std {
template <typename, typename = int> class Trans_NS___cxx11_basic_ostringstream;
class ios_base {
  class __attribute((__abi_tag__("cxx11"))) failure {
    virtual char m_fn2();
  };
};
class B : virtual ios_base {};
template <typename, typename> class Trans_NS___cxx11_basic_ostringstream : B {
public:
  void m_fn1();
};
}

class A {
public:
  A(int) {
    std::Trans_NS___cxx11_basic_ostringstream<wchar_t> a;
    a.m_fn1();
  }
};
int b;
void fn1() { (A(b)); }
int
main()
{
}
