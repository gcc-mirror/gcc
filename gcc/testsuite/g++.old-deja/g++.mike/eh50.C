// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf z8k-*-* arm-*-pe

#include <exception>

void my_unexpected() {
  throw 42;
}

template <class T> void foo(T) throw (int) { throw "Hi"; }

main() {
  std::set_unexpected (my_unexpected);
  try {
    foo(1);
  } catch (int i) {
    if (i == 42)
      return 0;
  }
  return 1;
}
