#include <stdlib.h>

struct foo
{
  char **m_f;
};

struct foo *
test (void)
{
  struct foo *new_table = (struct foo *) malloc(sizeof(struct foo));
  if (!new_table)
    return NULL;
  new_table->m_f = (char **)malloc(sizeof(char **));
  *new_table->m_f = NULL; /* { dg-warning "dereference of possibly-NULL '\\*new_table.m_f'" } */
  /* { dg-message "'\\*new_table.m_f' could be NULL" "final event wording" { target *-*-* } .-1 } */
  return new_table;
}
