// Special g++ Options: -O

#include <iostream.h>
#include <typeinfo>

int main() {
  int *i1, *i2;
  cerr << (typeid(i1)==typeid(i2)) << endl;
}
