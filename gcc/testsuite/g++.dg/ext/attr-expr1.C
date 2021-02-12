// PR c++/95192

template<typename T>
__attribute__((assume_aligned(sizeof(int(T()))))) // { dg-message "function type" }
T *f();

void test21() {
  void *p = f<void>()		// { dg-error "no match" }
}
