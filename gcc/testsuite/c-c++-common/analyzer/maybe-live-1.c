#include "analyzer-decls.h"

extern int unknown_int (void);

void
test_store_const_at_symbolic_index (int *p, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      p[i] = 42;
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
    }
}

void
test_store_unknown_at_symbolic_index (int *p, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      p[i] = unknown_int ();
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
    }
}

void
test_store_index_at_symbolic_index (int *p, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      p[i] = i;
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "3 processed enodes" } */
    }
}

void
test_store_index_squared_at_symbolic_index (int *p, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      p[i] = i * i;
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "3 processed enodes" } */
    }
}
