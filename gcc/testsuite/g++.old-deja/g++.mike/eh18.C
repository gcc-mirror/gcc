// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

class VB {
public:
  int n;
  VB (int v) { n = v; }
  VB (const VB& o) {
    n = o.n;
//    printf("copying VB from %d to %d\n", &o, this);
  }
};

class D : public virtual VB {
  int j;
public:
  D(int i1, int i2) : VB(i2) { j = i1; }
  VB& vb() { return *(VB*)this; }
  const VB& vb() const { return *(const VB*)this; }
};

class pD : private virtual VB {
  int j;
public:
  pD(int i1, int i2) : VB(i2) { j = i1; }
  VB& vb() { return *(VB*)this; }
  const VB& vb() const { return *(const VB*)this; }
};


int main () {
  D d(1943, 4279);
  pD pd(3621, 9527);
  VB *vb = &d.vb();
  VB *pvb = &pd.vb();

  // A catch of a public virtual base.
  try {
//    printf("Throwing D at %d (VB at %d)\n", &d, vb);
    throw d;
  }
  catch (VB& vb) {
//    printf("Catching VB at %d\n", &vb);
    if (vb.n != 4279)
      return 1;
  }
  catch (...) {
    return 1;
  }

  // A catch of a private virtual base.
  try {
//    printf("Throwing D at %d (VB at %d)\n", &pd, pvb);
    throw pd;
  }
  catch (VB& vb) {
//    printf("Catching VB at %d\n", &vb);
    // This was a private base of the throw object, don't catch it.
    return 1;
  }
  catch (...) {
  }
}
