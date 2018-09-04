// { dg-do compile }

class A {
    void m_fn1();
};

void A::m_fn1()
{
  A *a = this;
  for (int i; i && a;)
    a = 0;
}
