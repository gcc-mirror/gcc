// { dg-do compile { target c++11 } }
// { dg-additional-options "-fabi-version=20 -Wabi" }

struct Base {
protected:
  Base() = default;
  ~Base() = default;
};

struct Derived : Base {
  void* ptr;			// { dg-bogus "offset" }
};
