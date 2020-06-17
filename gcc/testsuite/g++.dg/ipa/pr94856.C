/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dse --param uninlined-function-insns=0 --param early-inlining-insns=3 -fgnu-tm " } */
/* { dg-require-effective-target fgnu_tm } */

class a {
public:
  virtual ~a() {}
};
class b {
public:
  virtual void c();
};
class C : a, public b {};
class d : C {
  ~d();
  void c();
};
d::~d() { ((b *)this)->c(); }
void d::c() {}
