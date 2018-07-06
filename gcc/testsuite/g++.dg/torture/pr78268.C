// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

typedef enum {} nsresult;

struct A {
      virtual nsresult m_fn1(bool);
};

struct B {
      A *operator[](int);
};

struct C {
      nsresult m_fn2(bool);
        bool m_fn3(bool);
	  B mDataSources;
};
nsresult C::m_fn2(bool p1)
{
  m_fn3(!p1);
}

bool C::m_fn3(bool p1)
{
  mDataSources[0]->m_fn1(p1);
}
