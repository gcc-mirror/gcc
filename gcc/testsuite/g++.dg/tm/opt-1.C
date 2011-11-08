// { dg-do compile }
// { dg-options "-fgnu-tm -O" }

struct S
{
  virtual void f() __attribute__((transaction_safe));
};

void f(S *s) { __transaction_atomic { s->f(); } }
