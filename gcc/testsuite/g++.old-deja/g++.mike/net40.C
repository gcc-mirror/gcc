#include <stddef.h>
#include <stdlib.h>

class toto {
public:
  void * operator new (size_t t) {
    abort();
  }
  void operator delete (void*p, size_t t) {
    abort();
  }
};

int main() {
  toto * p;
  p = new toto[5];
  delete [] p;
}
