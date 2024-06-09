#include <arm_sve.h>

svbool_t f1 (svcount_t x) { return x; } // { dg-error {cannot convert 'svcount_t' to 'svbool_t' in return} }
svcount_t f2 (svbool_t x) { return x; } // { dg-error {cannot convert 'svbool_t' to 'svcount_t' in return} }
void f3 (svbool_t *p, svcount_t x) { *p = x; } // { dg-error {cannot convert 'svcount_t' to 'svbool_t' in assignment} }
void f4 (svcount_t *p, svbool_t x) { *p = x; } // { dg-error {cannot convert 'svbool_t' to 'svcount_t' in assignment} }
svbool_t *f5 (svcount_t *p) { return p; } // { dg-error {cannot convert} }
svcount_t *f6 (svbool_t *p) { return p; } // { dg-error {cannot convert} }
svbool_t f7 (svcount_t x) { return (svbool_t) x; } // { dg-error {invalid cast from type 'svcount_t' to type 'svbool_t'} }
svcount_t f8 (svbool_t x) { return (svcount_t) x; } // { dg-error {invalid cast from type 'svbool_t' to type 'svcount_t'} }
