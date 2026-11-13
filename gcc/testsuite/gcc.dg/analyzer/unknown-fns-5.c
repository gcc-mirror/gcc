#include "analyzer-decls.h"

extern void unknown_side_effects (void);

void *global_ptr;

void
test_global_could_be_affected (void)
{
  __builtin_free (global_ptr);
  unknown_side_effects ();
  __builtin_free (global_ptr); /* { dg-bogus "double-'free' of 'global_ptr'" } */
}

void
test_param_cant_be_affected (void *p)
{
  __builtin_free (p);
  unknown_side_effects ();
  __builtin_free (p); /* { dg-warning "double-'free' of 'p'" } */
}

void *nonstatic_ptr;

void
test_nonstatic_ptr (void)
{
  __analyzer_describe (0, nonstatic_ptr); /* { dg-warning "svalue: 'INIT_VAL\\\(nonstatic_ptr\\\)'" } */
  unknown_side_effects ();
  __analyzer_describe (0, nonstatic_ptr); /* { dg-warning "svalue: 'UNKNOWN\\\(void \\\*\\\)'" } */
}

static void *static_ptr;

void
test_static_ptr (void)
{
  __analyzer_describe (0, static_ptr); /* { dg-warning "svalue: 'INIT_VAL\\\(static_ptr\\\)'" } */
  unknown_side_effects ();
  __analyzer_describe (0, static_ptr); /* { dg-warning "svalue: 'INIT_VAL\\\(static_ptr\\\)'" } */
}

int nonstatic_int = 42;

int
main (void)
{
  __analyzer_describe (0, nonstatic_int); /* { dg-warning "svalue: '\\\(int\\\)42'" } */
  unknown_side_effects ();
  __analyzer_describe (0, nonstatic_int); /* { dg-warning "svalue: 'UNKNOWN\\\(int\\\)'" } */
  return 0;
}
