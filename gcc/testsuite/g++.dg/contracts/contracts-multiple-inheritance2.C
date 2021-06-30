// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }

struct BaseA {
  virtual int fun(int n) [[ pre: n > 0 ]] { return -n; }
};

struct BaseB {
  virtual int fun(int n) [[ pre: n < 0 ]] { return -n; }
};

struct Child1 : public BaseA, BaseB {
  int fun(int n) [[ pre: n > 0 ]] { return -n; } // { dg-error "mismatched" }
};

struct Child2 : public BaseA, BaseB {
  int fun(int n) [[ pre: n < 0 ]] { return -n; } // { dg-error "mismatched" }
};

struct Child3 : public BaseA, BaseB {
  int fun(int n) { return -n; }
};

struct Child4 : public BaseA {
  int fun(int n);
};

int Child4::fun(int n)
  [[ pre: n != 0 ]] // { dg-error "mismatched" }
{
  return -n;
}

