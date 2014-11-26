// { dg-do run  }
// { dg-options "-O2" }
// prms-id: 4736

class Rep {
public:
  virtual int foo() { return 1; }
};

class Rep_1 : public Rep {
};

class VBaseMain {
public:
  virtual int foo() { return 2; }
};

class OtherVBase {
public:
  virtual int foo() { return 3; }
};

class Rep_2 : public Rep {
};

class DVBase : public VBaseMain, public Rep_2, public OtherVBase {
public:
  virtual int foo() { return 4; }
};

class Main : public Rep_1, virtual public DVBase {
public:
  virtual int foo() { return 5; }
};

int main() {
  Main m;
  if (m.foo() != 5)
    return 1;
  if (((Rep*)(Rep_1*)&m)->foo() != 5)
    return 2;
  if (((DVBase*)&m)->foo() != 5)
    return 3;
  if (((VBaseMain*)(DVBase*)&m)->foo() != 5)
    return 4;
  if (((Rep*)(Rep_2*)(DVBase*)&m)->foo() != 5)
    return 5;
  if (((OtherVBase*)(DVBase*)&m)->foo() != 5)
    return 6;
}
