// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for stdexcept" { ! hostedlib } }
#include <stdexcept>
class X : public std::runtime_error {
  X ();
};
