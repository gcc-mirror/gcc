// { dg-do run  }
// prms-id: 7651

int fail = 0;

class Foo {
public:
  Foo(double i) : data(i) { if (data != 1.0) fail = 1; }
  ~Foo() { if (data != 1.0) fail = 1; }
private:
  volatile double data;
};

int DingDong(double A) {
  volatile Foo a(A);

  if ( A != 0.0 ) {
    return 1;
  }
  return 0;
}


int main() {
  DingDong(1.0);
}
