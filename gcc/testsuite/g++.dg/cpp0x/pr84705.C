// { dg-do compile { target c++11 } }
// { dg-options "" }

int a[]{a};  // { dg-error "invalid conversion" }

template<int>
struct b {
  __attribute__((c([] {
    struct {
      int a = static_cast<struct d>(a);  // { dg-error "invalid use of incomplete type" }
    } e;
  })));
};
