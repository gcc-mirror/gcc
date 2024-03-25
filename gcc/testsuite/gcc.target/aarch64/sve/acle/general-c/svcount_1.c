#include <arm_sve.h>

svbool_t f1 (svcount_t x) { return x; } /* { dg-error {incompatible types} } */
svcount_t f2 (svbool_t x) { return x; } /* { dg-error {incompatible types} } */
void f3 (svbool_t *p, svcount_t x) { *p = x; } /* { dg-error {incompatible types} } */
void f4 (svcount_t *p, svbool_t x) { *p = x; } /* { dg-error {incompatible types} } */
svbool_t *f5 (svcount_t *p) { return p; } /* { dg-error {incompatible return type} } */
svcount_t *f6 (svbool_t *p) { return p; } /* { dg-error {incompatible return type} } */
svbool_t f7 (svcount_t x) { return (svbool_t) x; } /* { dg-error {conversion to non-scalar} } */
svcount_t f8 (svbool_t x) { return (svcount_t) x; } /* { dg-error {conversion to non-scalar} } */
