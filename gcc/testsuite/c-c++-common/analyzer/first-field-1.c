#include "../../gcc.dg/analyzer/analyzer-decls.h"

typedef struct base_obj
{
  int m_first;
  int m_second;
} base_obj;

typedef struct sub_obj
{
  base_obj base;
} sub_obj;

void test (sub_obj *sub)
{
  sub->base.m_first = 1;
  sub->base.m_second = 2;
  __analyzer_eval (sub->base.m_first == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (sub->base.m_second == 2); /* { dg-warning "TRUE" } */

  base_obj *base = (struct base_obj *)sub;
  __analyzer_eval (base->m_first == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (base->m_second == 2); /* { dg-warning "TRUE" } */
}
