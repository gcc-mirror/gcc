// { dg-do run  }
// { dg-options "-w" }

class A {
public:
  virtual ~A(){};
  virtual int type(void) {
    return -1;
  }
};

class B : public A {
public:
  virtual ~B(){};
};


class C0 : public B, public virtual A {
public:
  virtual int type(void) {
    return 0;
  }
};

class C1 : public C0
{
public:
  virtual int type(void) {
    return 1;
  }
};

class C2 : public C0 {
public:
  virtual int type(void) {
    return 2;
  }
};

main() {
  C1 *one = new C1;
  
  if (one->type() != 1)
    return 1;
}
