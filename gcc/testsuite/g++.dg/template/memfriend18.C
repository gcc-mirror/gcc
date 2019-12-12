// PR c++/87571

template <class> struct A {
  template <class> struct B {
    template <class> friend class B;
  protected:
    int protected_member_;
  public:
    template <class T> int method(const B<T>& other) const {
      return other.protected_member_;
    }
  };
};

int main() {
  A<int>::B<int> a;
  A<int>::B<char> b;
  a.method(b);
}
