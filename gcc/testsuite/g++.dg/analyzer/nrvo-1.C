#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct s1
{
  s1 (int x) : m_x (x) {}
  int m_x;
};

s1 make_s1 (int x)
{
  return s1 (x);
}

void test_1 (int x)
{
  s1 s = make_s1 (x);
  __analyzer_eval (s.m_x == x); // { dg-warning "TRUE" }
}
