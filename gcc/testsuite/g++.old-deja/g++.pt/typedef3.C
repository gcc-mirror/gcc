// { dg-do assemble  }

template <class T>
void f(T, T)
{
}

struct A {
  typedef enum {
    VAL1
  } result_t;
};

struct B {
  typedef enum {
    VAL2
  } result_t;
};


void g()
{
  f(A::VAL1, A::VAL1);
  f(B::VAL2, B::VAL2);
}
