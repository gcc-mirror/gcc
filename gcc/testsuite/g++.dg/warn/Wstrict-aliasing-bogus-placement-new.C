/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing" } */

inline void *operator new (__SIZE_TYPE__, void *__p) throw() { return __p; }

struct Y {
  Y() {}
  int i;
};

struct X {
  X() {}
  void construct(const Y& y)
  {
    new (&m_data[0]) Y(y);
  }
  bool initialized;
  char m_data[sizeof (Y)];
};

void bar(const X&);
void foo(Y& y)
{
  X x;
  x.construct(y);
  x.initialized = true;
  bar(x);
}

