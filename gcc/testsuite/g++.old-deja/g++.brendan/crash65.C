// { dg-do assemble  }
// GROUPS passed old-abort
class X {
public:  
  virtual const char* 	XY(const void* val) const = 0;
};


class Y : public X {
public:
  using X::xy;// { dg-error "" }  no memb.*

  using X::z;// { dg-error "" }  no memb.*
};
