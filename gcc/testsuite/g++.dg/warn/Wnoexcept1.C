// PR c++/90992
// { dg-do compile { target c++11 } }
// { dg-additional-options -Wnoexcept }

#include "Wnoexcept1.h"

// We expect a warning at the declaration of construct2, since Automatic2 is
// defined below; we don't expect one for construct1, because Automatic1 is
// defined in the fake system header.
// { dg-warning "noexcept-expression" "" { target *-*-* } 16 }

class Automatic2 {
public:
  Automatic2(size_t bla) : Bla(bla) {}; // { dg-message "noexcept" }

private:
  size_t Bla;
};

union U
{
  unsigned char buf[sizeof(Automatic1)];
  Automatic1 a1;
  Automatic2 a2;
  U(): buf{} {}
  ~U() {}
};

int main() {
  U u;
  construct1(&u.a1, 42);
  construct2(&u.a2, 42);
}
