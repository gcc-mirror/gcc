#include <stdint.h>
#include "analyzer-decls.h"

void
test_all_ones_int32_t (void)
{
  int32_t v = -1;
  __analyzer_describe (0, v); /* { dg-warning "svalue: '\\\(int32_t\\\)-1'" } */
}

void
test_all_ones_uint32_t (void)
{
  uint32_t v = -1;
  __analyzer_describe (0, v); /* { dg-warning "svalue: '\\\(uint32_t\\\)4294967295'" } */
}

void
test_all_ones_int64_t (void)
{
  int64_t v = -1;
  __analyzer_describe (0, v); /* { dg-warning "svalue: '\\\(int64_t\\\)-1'" } */
}

void
test_all_ones_uint64_t (void)
{
  uint64_t v = -1;
  __analyzer_describe (0, v); /* { dg-warning "svalue: '\\\(uint64_t\\\)18446744073709551615'" } */
}

void
test_nearly_all_ones_uint64_t (void)
{
  uint64_t v = 0xfffffffffffffffeULL;
  __analyzer_describe (0, v); /* { dg-warning "svalue: '\\\(uint64_t\\\)18446744073709551614'" } */
}
