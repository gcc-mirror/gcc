/* { dg-do compile } */

class a {
public:
  void *operator new(__SIZE_TYPE__, int);
};
class b : public a {
public:
  b();
  virtual void c() {}
  void m_fn2();
};
int d;
void e() {
  long f;
  b *g;
  (f);
  g = new (d) b;
  g->c();
  g->m_fn2();
}
