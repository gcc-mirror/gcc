// PR c++/66130

struct X {
  X(void *);
  void m();  // { dg-message "declared here" }
};

struct Y : public X{
  Y(void*a, void *b) : X(m), mb(b) { }  // { dg-error "member function 'void X::m\\(\\)'" }
  void *mb;
};
