  #include <vector>

  template <typename T=float> class foo {
  public:
    foo();
    foo(vector<int> v);
  private:
    vector<int> v;
    T t;
  };

  template <typename T=float> foo<T>::foo()               :v(),   t() {}
  template <typename T=float> foo<T>::foo(vector<int> v_) :v(v_), t() {}

  foo<float> a;
