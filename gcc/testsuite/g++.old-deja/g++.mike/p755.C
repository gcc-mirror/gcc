// It checks to see if you can define your own global new operator.
// prms-id: 755

#include <new>
extern "C" void exit(int);

void* operator new(size_t sz) throw (std::bad_alloc) {
  void* p = 0;
  exit(0);
  return p;
}

int main () {
  int* i = new int;
  delete i;
  return 1;
}
