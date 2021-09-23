#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct A
{
  int m_data;
  virtual char foo ()
  {
    return 'A';
  }
};

struct B: public A
{
  int m_data_b;
  char foo ()
  {
    return 'B';
  }
};

void test()
{
  A a, *a_ptr = &a;
  B b;
  __analyzer_eval (a_ptr->foo () == 'A'); /* { dg-warning "TRUE" } */
  a_ptr = &b;
  __analyzer_eval (a_ptr->foo () == 'B'); /* { dg-warning "TRUE" } */
}
