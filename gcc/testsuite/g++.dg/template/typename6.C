struct O {
  template <typename T>
  struct I {
    I (int);
  };
};

template <typename T>
void f() {
  typename ::O::I<int>(3);
}
