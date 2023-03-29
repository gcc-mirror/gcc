// PR c++/109042
// { dg-do compile }

namespace std { class type_info {}; }

std::type_info
foo ()
{
  return typeid (void);
}

namespace __cxxabiv1 {
  struct __fundamental_type_info {
    virtual ~__fundamental_type_info ();
  };

  __fundamental_type_info::~__fundamental_type_info ()
  {
  }
}
