// PR sanitizer/88901
// { dg-do compile }
// { dg-options "-fsanitize=address -fsanitize=pointer-compare" }

template <typename T>
struct A {
  void foo() {
    auto d = [](char *x, char *y) {
      for (char *p = x; p + sizeof(T) <= y; p += sizeof(T))
        reinterpret_cast<T *>(p)->~T();
    };
  }
};
