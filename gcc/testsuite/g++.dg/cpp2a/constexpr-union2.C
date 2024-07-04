// PR c++/101631
// { dg-do compile { target c++20 } }

struct sso {
  union {
    int buf[10];
    int* alloc;
  };
};

constexpr bool direct() {
  sso val;
  val.alloc = nullptr;
  val.buf[5] = 42;
  return true;
}
constexpr bool ok = direct();


constexpr void perform_assignment(int& left, int right) noexcept {
  left = right;  // { dg-error "accessing .+ member instead of initialized" }
}

constexpr bool indirect() {
  sso val;
  val.alloc = nullptr;
  perform_assignment(val.buf[5], 42);  // { dg-message "in .constexpr. expansion" }
  return true;
}
constexpr bool err = indirect();  // { dg-message "in .constexpr. expansion" }
