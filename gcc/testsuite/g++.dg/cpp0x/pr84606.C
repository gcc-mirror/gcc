// { dg-do compile { target c++11 } }

template<typename>
struct s {
  struct {
    void __attribute__((common([] { struct d }))) g();  // { dg-error "expected|attribute" }
  } f;
};

s<int> a {};
