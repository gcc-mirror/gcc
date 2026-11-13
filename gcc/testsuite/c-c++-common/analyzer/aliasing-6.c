/* Various tests that initial values of pointers can't alias
   heap allocations, where those initial values are derived
   from various sources.  */

#include "analyzer-decls.h"

struct foo { int *add_info; };
struct s_zero_offset { struct foo *start; };
struct s_nonzero_offset { unsigned long long pad; struct foo *start; };

struct s_zero_offset g_zero_offset;
struct s_nonzero_offset g_nonzero_offset;
struct foo *gp;

void
test_field_of_global_zero_offset (void)
{
  struct foo *p = g_zero_offset.start; struct foo **q = &p; (void) q;
  int *a = (int *)__builtin_malloc (sizeof (int));
  __analyzer_eval (p == g_zero_offset.start); /* { dg-warning "TRUE" } */
  p->add_info = a;
  __analyzer_eval (p->add_info == a); /* { dg-warning "TRUE" } */
  p->add_info = 0;
} /* { dg-warning "leak of 'a'" } */

void
test_field_of_global_nonzero_offset (void)
{
  struct foo *p = g_nonzero_offset.start; struct foo **q = &p; (void) q;
  int *a = (int *)__builtin_malloc (sizeof (int));
  __analyzer_eval (p == g_nonzero_offset.start); /* { dg-warning "TRUE" } */
  p->add_info = a;
  __analyzer_eval (p->add_info == a); /* { dg-warning "TRUE" } */
  p->add_info = 0;
} /* { dg-warning "leak of 'a'" } */

void
test_ssa_local (void)
{
  struct foo *p = g_zero_offset.start;
  int *a = (int *)__builtin_malloc (sizeof (int));
  __analyzer_eval (p->add_info == a); /* { dg-warning "UNKNOWN" } */
  p->add_info = a;
  __analyzer_eval (p->add_info == a); /* { dg-warning "TRUE" } */
  p->add_info = 0;
} /* { dg-warning "leak of 'a'" } */

void
test_whole_global (void)
{
  struct foo *p = gp; struct foo **q = &p; (void) q;
  int *a = (int *)__builtin_malloc (sizeof (int));
  __analyzer_eval (p == gp); /* { dg-warning "TRUE" } */
  p->add_info = a;
  __analyzer_eval (p->add_info == a); /* { dg-warning "TRUE" } */
  p->add_info = 0;
} /* { dg-warning "leak of 'a'" } */

void
test_from_param (struct foo *p0)
{
  struct foo *p = p0; struct foo **q = &p; (void) q;
  int *a = (int *)__builtin_malloc (sizeof (int));
  __analyzer_eval (p == p0); /* { dg-warning "TRUE" } */
  p->add_info = a;
  __analyzer_eval (p->add_info == a); /* { dg-warning "TRUE" } */
  p->add_info = 0;
} /* { dg-warning "leak of 'a'" } */

struct foo *pg;

void
test_pointer_in_global (void)
{
  pg = g_zero_offset.start;
  int *a = (int *)__builtin_malloc (sizeof (int));
  pg->add_info = a;
  __analyzer_eval (pg->add_info == a); /* { dg-warning "UNKNOWN" } */
  pg->add_info = 0;
}
