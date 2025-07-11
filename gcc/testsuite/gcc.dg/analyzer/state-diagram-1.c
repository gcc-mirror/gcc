/* { dg-additional-options "-fdiagnostics-add-output=sarif:state-graphs=yes" } */

#include "analyzer-decls.h"

struct node
{
  struct node *m_next;
  int m_val;
};

struct node *first;

struct node *
append_value (int value)
{
  struct node *n = __builtin_malloc (sizeof (struct node));
  if (!n)
    return 0;
  n->m_val = value;
  
  n->m_next = first;
  first = n;

  return n;
}

int g;

void
test ()
{
  if (!append_value (42))
    return;
  if (!append_value (1066))
    return;
  if (!append_value (1776))
    return;

  __builtin_free (first->m_next->m_next);
  first->m_next->m_next->m_next->m_next = NULL;  /* { dg-warning "-Wanalyzer-use-after-free" } */
}

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest state-diagram-1.c "state-diagram-1-sarif.py" } } */
