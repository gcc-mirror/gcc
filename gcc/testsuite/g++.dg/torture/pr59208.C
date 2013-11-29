// { dg-do compile }
class A {
public:
  A();
  A(int *);
};
class B {};
class C : B {
public:
  virtual void m_fn1();
  void operator+=(int) { m_fn1(); }
};
enum DebuggerType {};
C a;
DebuggerType b;
void operator==(A &, const A &);
static A get_dbx_doc(A &p1) { p1 == 0; }

void add_button() {
  A c;
  switch (b)
  case 0:
  get_dbx_doc(c);
  a += 0;
}
