// { dg-do run }
// { dg-options "-w -fabi-version=0" }

#include <limits>

union U {
  int i: 4096;
};

int main () {
  if (sizeof (U) * std::numeric_limits<unsigned char>::digits != 4096)
    return 1;
}

