// { dg-lto-do link }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-lto-options {{-fPIC -shared -flto}} }
// { dg-extra-ld-options "-Wl,-undefined,dynamic_lookup" { target *-*-darwin* } }

class VclReferenceBase { 
  int mnRefCnt;
  int mbDisposed : 3;
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
