// { dg-lto-do link }
// { dg-require-effective-target lto_incremental }
// { dg-lto-options { { -flto -O2 } } }
// { dg-extra-ld-options "-r -nostdlib -flto -flto-partition=none -O2" }
namespace xercesc_3_1 {
class XMLEntityHandler {
public:
  virtual ~XMLEntityHandler();
  virtual void m_fn1();
  virtual bool m_fn2();
  virtual void m_fn3();
  virtual int m_fn4();
  virtual void m_fn5();
} * a;
void fn1() {
  a->m_fn5();
  a->m_fn1();
}
}

