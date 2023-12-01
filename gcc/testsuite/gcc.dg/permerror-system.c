/* { dg-options "-isystem ${srcdir}" } */

/* Test that permerrors appear in system headers.  */

/* The dg-* directives in the header file are ignored.  No warnings are
   expected.  */
#include <gcc.dg/permerror-default.c>

/* These errors come from permerror-default.c.  */

/* { dg-error "'f1' \\\[-Wimplicit-function-declaration\\\]" "" { target *-*-* } 10 } */

/* { dg-error "type of 'i' defaults to 'int' \\\[-Wimplicit-int\\\]" "" { target *-*-*} 16 } */

/* { dg-error "pointer/integer type mismatch in conditional expression \\\[-Wint-conversion\\\]" "" { target *-*-* } 29 } */
/* { dg-error "pointer/integer type mismatch in conditional expression \\\[-Wint-conversion\\\]" "" { target *-*-* } 30 } */
/* { dg-error "passing argument 1 of 'f2' makes pointer from integer without a cast \\\[-Wint-conversion\\\]" "" { target *-*-* } 31 } */
/* { dg-error "initialization of 'int' from 'int \\\*' makes integer from pointer without a cast \\\[-Wint-conversion\\\]" "" { target *-*-* } 33 } */
/* { dg-error "assignment to 'int' from 'int \\\*' makes integer from pointer without a cast \\\[-Wint-conversion\\\]" "" { target *-*-* } 34 } */
/* { dg-error "returning 'int' from a function with return type 'int \\\*' makes pointer from integer without a cast \\\[-Wint-conversion\\\]" "" { target *-*-* } 36 } */
/* { dg-error "passing argument 1 of 'f3' makes integer from pointer without a cast \\\[-Wint-conversion\\\]" "" { target *-*-* } 43 } */
/* { dg-error "initialization of 'int \\\*' from 'int' makes pointer from integer without a cast \\\[-Wint-conversion\\\]" "" { target *-*-* } 45 } */
/* { dg-error "assignment to 'int \\\*' from 'int' makes pointer from integer without a cast \\\[-Wint-conversion\\\]" "" { target *-*-* } 46 } */
/* { dg-error "returning 'int \\\*' from a function with return type 'int' makes integer from pointer without a cast \\\[-Wint-conversion\\\]" "" { target *-*-* } 48 } */
