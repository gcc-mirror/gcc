// Build don't link:
// Special g++ Options: -Wno-pmf-conversions
// prms-id: 11116

class Bar {
public:
  int f(int a) { val = a; return val; }
private:
  int val;
};

typedef int (Bar::*BarPtr)(int);

void foo() {
  int a;
  int (Bar::*bp)(int) = &Bar::f;
  Bar bar;
  int (*p)(void *, int);

  p = (int (*)(void*,int))((void (*)())((bp).__pfn_or_delta2.__pfn));
  a = (*p)(&bar, 4);
}
