// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

#include <typeinfo>

class B {
public:
  int data;
  B(int i) : data(i) {
  }
} b(42);

main() {
  try {
    throw &b;
  } catch (const B* bptr) {
    if (bptr->data == 42)
      {
	try {
	  throw &b;
	} catch (void *bptr) {
	  if (((B*)bptr)->data == 42)
	    return 0;
	}
      }
  }
  return 1;
}
