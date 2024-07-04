// PR c++/113158
// { dg-do compile { target c++11 } }

template<typename T>
struct V {
  static constexpr bool t = false;
};
struct base {
    virtual int f() = 0;
};

template<typename T>
struct derived : base {
    int f() noexcept(V<T>::t) override;
};

struct base2 {
    virtual int f() noexcept = 0;
};

template<bool B>
struct W {
  static constexpr bool t = B;
};

template<bool B>
struct derived2 : base2 {
    int f() noexcept(W<B>::t) override; // { dg-error "looser exception specification" }
};

void
g ()
{
  derived<int> d1;
  derived2<false> d2; // { dg-message "required from here" }
  derived2<true> d3;
}
