// { dg-do run  }
class base {
public:
  virtual ~base() {}
  virtual void m1() = 0;
  virtual void m2() = 0;
};

class intermediate : public virtual base {
public:
  virtual ~intermediate() {}
  virtual void m1() {}
  virtual void m2() {}
};

class derived : public intermediate {
public:
  virtual int IwantedThisMethod() { return 0; }
  virtual int ButIgotThisOne() { return 1; }
};

int main() {
  return derived().IwantedThisMethod();
}
