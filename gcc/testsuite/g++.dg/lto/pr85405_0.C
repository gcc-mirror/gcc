// { dg-lto-do link }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-lto-options {{-fPIC -shared -flto}} }

class VclReferenceBase { // { dg-lto-warning "7: type 'struct VclReferenceBase' violates the C\\+\\+ One Definition Rule" }
  int mnRefCnt;
  bool mbDisposed : 1;
  virtual ~VclReferenceBase();
};
class a;
class b {
  a &e;
  bool c();
};
class B {
  VclReferenceBase d;
};
class a : B {};
bool b::c() { return false; }
