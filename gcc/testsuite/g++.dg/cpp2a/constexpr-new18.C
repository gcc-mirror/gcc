// PR c++/99859
// { dg-do compile { target c++20 } }

template <class T>
struct intrusive_ptr
{
  T *ptr = nullptr;
  constexpr explicit intrusive_ptr(T* p) : ptr(p) {
    ++ptr->count_;
  }
  constexpr ~intrusive_ptr() {
    if (ptr->dec() == 0)
      delete ptr;
  }
  constexpr intrusive_ptr(intrusive_ptr const& a) : ptr(a.ptr) {
    ++ptr->count_;
  }
};

struct Foo {
  int count_ = 0;
  constexpr int dec() {
    return --count_;
  }
};

constexpr bool baz() {
  Foo f { 4 };
  intrusive_ptr a(&f);
  return true;
}
constexpr bool x = baz();

constexpr void bar(intrusive_ptr<Foo> a) 
{
  if (a.ptr->count_ != 2) throw 1;
}

constexpr bool foo() {
  intrusive_ptr a(new Foo());
  bar(a);
  return true;
}

static_assert(foo());
