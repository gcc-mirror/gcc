// Build don't link: 
// GROUPS passed old-abort
class X {
public:  
  virtual const char* 	XY(const void* val) const = 0;
};


class Y : public X {
public:
  using X::xy;// ERROR -  no memb.*

  using X::z;// ERROR -  no memb.*
};
