// PR c++/21495
// { dg-do compile }

class A
{
  extern void *copy (void) // { dg-error "storage class specified" }
  {
    return 0;
  }
  extern A &operator= (const A &) // { dg-error "storage class specified" }
  {
    return *this;
  }
};
