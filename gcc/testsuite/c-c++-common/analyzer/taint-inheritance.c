#include "analyzer-decls.h"
#include <stdint.h>

struct ab { int i; int j; };

void __attribute__((tainted_args))
test_by_value (struct ab s)
{
  __analyzer_dump_state ("taint", s); /* { dg-warning "tainted" } */
  __analyzer_dump_state ("taint", s.i); /* { dg-warning "tainted" } */
  __analyzer_dump_state ("taint", s.j); /* { dg-warning "tainted" } */
}

void __attribute__((tainted_args))
test_by_ptr (struct ab *p)
{
  __analyzer_dump_state ("taint", p); /* { dg-warning "tainted" } */
  __analyzer_dump_state ("taint", p->i); /* { dg-warning "tainted" } */
  __analyzer_dump_state ("taint", p->j); /* { dg-warning "tainted" } */
}

void __attribute__((tainted_args))
test_memset (char c)
{
  char buf[10];
  __builtin_memset (buf, c, sizeof (buf));
  __analyzer_dump_state ("taint", c); /* { dg-warning "tainted" } */
  __analyzer_dump_state ("taint", buf[0]); /* { dg-warning "tainted" } */
  __analyzer_dump_state ("taint", buf[9]); /* { dg-warning "tainted" } */
}

void __attribute__((tainted_args))
test_concat (char c0, char c1, char c2, char c3)
{
  char buf[4];
  buf[0] = c0; buf[1] = c1; buf[2] = c2; buf[3] = c3;
  __analyzer_dump_state ("taint", buf[0]); /* { dg-warning "tainted" } */
  __analyzer_dump_state ("taint", *(int32_t *)buf); /* { dg-warning "tainted" "ideal" { xfail *-*-* } } */
  /* { dg-bogus "start" "status quo" { xfail *-*-* } .-1 } */
}

void __attribute__((tainted_args))
test_crop_scalar (int32_t v)
{
  char buf[4];
  *(int32_t *)buf = v;
  __analyzer_dump_state ("taint", v); /* { dg-warning "tainted" } */
  __analyzer_dump_state ("taint", buf[0]); /* { dg-warning "tainted" } */
}

void __attribute__((tainted_args))
test_has_ub (int32_t v)
{
  char buf[4];
  if (v > 100)
    return;
  __analyzer_dump_state ("taint", v); /* { dg-warning "has_ub" } */
  *(int32_t *)buf = v;
  __analyzer_dump_state ("taint", buf[0]); /* { dg-warning "has_ub" } */
}

void __attribute__((tainted_args))
test_struct_copy (struct ab *p)
{
  struct ab local = *p;
  __analyzer_dump_state ("taint", p->i); /* { dg-warning "tainted" } */
  __analyzer_dump_state ("taint", local.i); /* { dg-warning "tainted" } */
}
