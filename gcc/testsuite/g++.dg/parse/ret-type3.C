// PR c++/21369

struct bar;

template <class T> struct bar *foo (T *p)
{
  return p->t;
}
