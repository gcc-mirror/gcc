// { dg-do run  }
// prms-id: 3068

extern "C" int printf(const char *, ...);
extern "C" void exit(int);
 
class LB {
public:
  virtual int test() { return 0; }
  virtual ~LB() { }
protected:
  LB() { }
};

class RRB {
public:
  virtual ~RRB() { }
  virtual void test2(int a) { }
};

class RR : public RRB {
public:
    virtual ~RR() { }
};

class RL {
public:
  virtual void real(int a) {
    printf("RL::real\n");
  }
};


class R : public RL, public RR {
public:
  virtual void test3(int a) { }
  virtual void test2(int a) { }
};

class L : public LB {
};

class C : public L, public R {
public:
  C() { }
  virtual ~C() {
    printf("C::~C\n");
    exit(1);
  }
  virtual void real(int a) {
    printf("RL::real\n");
  }
};

int main() {
    C& bb = *new C;
    R& mv = bb;
    bb.real(0);
    mv.real(0);
    return 0;
}
