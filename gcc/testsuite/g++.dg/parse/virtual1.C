// PR c++/71099

struct A {
  virtual void foo();
};

virtual void A::foo() {}  // { dg-error "'virtual' outside class" }

template<typename>
struct B {
  virtual void foo();
};

template<typename T>
virtual void B<T>::foo() {}  // { dg-error "'virtual' outside class" }

struct C {
  template<typename>
  virtual void foo();  // { dg-error "templates may not be 'virtual'" }
};

template<typename>
virtual void C::foo() {}  // { dg-error "'virtual' outside class" }

template<typename>
struct D {
  template<typename>
  virtual void foo();  // { dg-error "templates may not be 'virtual'" }
};

template<typename T>
template<typename>
virtual void D<T>::foo() {}  // { dg-error "'virtual' outside class" }
