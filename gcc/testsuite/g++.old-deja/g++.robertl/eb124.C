// { dg-do assemble  }
#include <stdexcept>
class X : public std::runtime_error {
  X ();
};
