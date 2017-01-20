// { dg-do run { target c++11 } }
// PR78495 failed to propagate pass-by-value struct to base ctor.

struct Ptr {
  void *ptr = 0;

  Ptr() {}
  Ptr(Ptr const&) = delete;
  Ptr(Ptr&& other) : ptr (other.ptr) {}
};

struct Base {
  Ptr val;
  Base(Ptr val_) : val(static_cast<Ptr&&>(val_)) {}
};

struct Derived: Base {
  using Base::Base;
};

void *Foo () {
  Ptr ptr;

  Derived d(static_cast<Ptr&&>(ptr));

  return d.val.ptr;
}

int main () {
  return Foo () != 0;
}
