// PR c++/57352

struct x
{
  operator class {} ();  // { dg-error "types|expected" }
};
