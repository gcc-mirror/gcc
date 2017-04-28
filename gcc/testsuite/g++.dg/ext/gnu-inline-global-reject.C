/* Test __attribute__((gnu_inline)).

   Check that we reject various forms of duplicate definitions.
*/

/* { dg-do compile } */
/* { dg-options " -ansi -Wno-long-long -ftrack-macro-expansion=0" } */

#include "gnu-inline-common.h"

#undef fn
#define fn pfx(func_decl_inline_before)
decl(inline, fn) // { dg-message "previous" }
gnuindef(fn, 0) // { dg-error "redeclared" }

#undef fn
#define fn pfx(func_decl_inline_after)
gnuindef(fn, 0) // { dg-message "previous" }
decl(inline, fn) // { dg-error "redeclared" }

#undef fn
#define fn pfx(func_def_gnuin_redef)
gnuindef(fn, 0) // { dg-message "previous" }
gnuindef(fn, 1) // { dg-error "redefinition" }

#undef fn
#define fn pfx(func_def_inline_redef)
def(inline, fn, 0) // { dg-message "previous" }
def(inline, fn, 1) // { dg-error "redefinition" }

#undef fn
#define fn pfx(func_def_inline_after)
gnuindef(fn, 0) // { dg-message "previous" }
def(inline, fn, 1) // { dg-error "redeclare" }

#undef fn
#define fn pfx(func_def_inline_before)
def(inline, fn, 0) // { dg-message "previous" }
gnuindef(fn, 1) // { dg-error "redefinition" }

#undef fn
#define fn pfx(func_def_before)
def(, fn, 0) // { dg-message "previous" }
gnuindef(fn, 1) // { dg-error "redefinition" }

#undef fn
#define fn pfx(func_decl_static_inline_before)
decl(static inline, fn) // { dg-message "previous" }
gnuindef(fn, 0) // { dg-error "redeclared" }

#undef fn
#define fn pfx(func_def_static_inline_after)
decl(static, fn)
gnuindef(fn, 0) // { dg-message "previous" }
decl(static, fn)
def(static inline, fn, 1) // { dg-error "redeclare" }
