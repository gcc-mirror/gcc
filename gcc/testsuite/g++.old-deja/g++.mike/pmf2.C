// { dg-do run  }
extern "C" int printf(const char *, ...);

class A_table {
  int c;
public:
  A_table() { c = 3;}
  virtual void func2(int &item) { printf("func2(%d,) c=%d\n",item,c);}
};

class B_table : private A_table {
public:
  typedef void (B_table::* B_ti_fn) (int &item);
  B_table() { j = 0x4321;}
  virtual void call_fn_fn1(int &item, void *pfn1);
  void func1(int &item) { printf("func1(%d)\n",item);}
  virtual void func2(int &item) { printf("func2(%d) j=%d\n",item,j);}
  int j;
};

class foo : public A_table {
public:
  int i;
  virtual ~foo();
  virtual void func2(int &item) { printf("func2(%d) i=%d\n",item,i);}
};
foo::~foo() { i = 0;}

class bar :public foo,public B_table {
public:
  int w;
  virtual ~bar();
  virtual void func2(int &item) { printf("func2(%d) w=%d\n",item,w);}
};
bar::~bar() { w = 0;}

void B_table::call_fn_fn1(int &item, void *pfn1) {
  (this->*(*(B_ti_fn*)pfn1))(item);
}

B_table b;
bar jar;

int main() {
  printf("ptr to B_table=%x, ptr to A_table=%x\n",&b,(A_table*)&b);
  B_table::B_ti_fn z = &B_table::func1;
  int j = 1;
  jar.call_fn_fn1(j,(void *)&z);
  j++;
  z = &B_table::func2;
  b.call_fn_fn1(j,(void *)&z);
  j++;
  jar.call_fn_fn1(j,(void *)&z);
  return 0;
}
