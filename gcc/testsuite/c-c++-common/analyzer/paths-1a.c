#include "analyzer-decls.h"

union foo
{
  int m_flag;
};

extern void bar (int);

void test (union foo *pf)
{
  if (pf->m_flag)
    bar (0);
  else
    bar (1);
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
