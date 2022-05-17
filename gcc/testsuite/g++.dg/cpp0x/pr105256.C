// PR c++/105256
// { dg-do compile { target c++11 } }

int bar (int &);

struct S {
  struct T {
    struct U {
      int i = bar (i);
    } u;
  };
};

void
foo (S::T *p)
{
  *p = {};
}
