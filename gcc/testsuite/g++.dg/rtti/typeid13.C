// PR c++/92009

namespace std {
  class type_info {};
}

bool
a2 ()
{
  return ((void **) &typeid (int))[0];
}
