// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontracts-nonattr-inheritance-mode=P2900R13" }

bool y_f4_called = false;
bool y2_f4_called = false;
bool y3_f4_called = false;
bool y4_f4_called = false;

struct X {
  int f3() {return 42;}

  virtual void f4(int x);
};

void g3(X* x)
{
  x->f4(x->f3());
}

struct Y : X {
  void f4(int x) override {
    y_f4_called = true;
  }
};

struct X2 {
  int f3() post(r: r ==42) {return 42;}
  virtual void f4(int x) pre(x == 42) = 0;
  virtual ~X2();
};

void g4(X2* x)
{
  x->f4(x->f3());
}

struct Y2 : X2 {
  void f4(int x) override {
    y2_f4_called = true;
  }
};

struct X3 {
  int f3() {return 42;}

  virtual int f4(int x);
};

void g5(X3* x)
{
  x->f4(x->f3());
}

struct Y3 : X3 {
  int f4(int x) override {
    y3_f4_called = true;
    return 666;
  }
};

struct X4 {
  int f3() post(r: r ==42) {return 42;}
  virtual int f4(int x) pre(x == 42) = 0;
  virtual ~X4();
};

void g6(X4* x)
{
  x->f4(x->f3());
}

struct Y4 : X4 {
  int f4(int x) override {
    y4_f4_called = true;
    return 666;
  }
};

int main()
{
    Y2 y2;
    g4(&y2);
    Y y;
    g3(&y);
    Y3 y3;
    g5(&y3);
    Y4 y4;
    g6(&y4);
    contract_assert(y_f4_called);
    contract_assert(y2_f4_called);
    contract_assert(y3_f4_called);
    contract_assert(y4_f4_called);
}

X2::~X2() {}
X4::~X4() {}
void X::f4(int x) {}
int X3::f4(int x) { return 42;}
