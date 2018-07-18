// { dg-do run { target c++11 } }
// PR78495 failed to propagate pass-by-value struct to base ctor.

static int moves = 0;

struct Ptr {
  void *ptr = 0;

  Ptr() {}
  Ptr(Ptr const&) = delete;
  Ptr(Ptr&& other) : ptr (other.ptr) {moves++;}
};

struct Base {
  Ptr val;
  Base(Ptr val_);
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
  if (Foo ())
    return 1;

  if (moves != 2)
    return 2;

  return 0;
}

Base::Base(Ptr val_) : val(static_cast<Ptr&&>(val_)) {}
