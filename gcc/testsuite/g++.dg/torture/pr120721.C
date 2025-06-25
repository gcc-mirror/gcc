// { dg-additional-options "-w -fno-vect-cost-model" }

template <int __v> struct integral_constant {
  static constexpr int value = __v;
};
template <bool __v> using __bool_constant = integral_constant<__v>;
template <bool> using enable_if_t = int;
struct function_ref {
  template <typename Callable>
  function_ref(
      Callable,
      enable_if_t<__bool_constant<__is_same(int, int)>::value> * = nullptr);
};
struct ArrayRef {
  int Data;
  long Length;
  int *begin();
  int *end();
};
struct StringRef {
  char Data;
  long Length;
};
void attributeObject(function_ref);
struct ScopedPrinter {
  virtual void printBinaryImpl(StringRef, StringRef, ArrayRef, bool, unsigned);
};
struct JSONScopedPrinter : ScopedPrinter {
  JSONScopedPrinter();
  void printBinaryImpl(StringRef, StringRef, ArrayRef Value, bool,
                       unsigned StartOffset) {
    attributeObject([&] {
      StartOffset;
      for (char Val : Value)
        ;
    });
  }
};
JSONScopedPrinter::JSONScopedPrinter() {}
