/* { dg-options "-isystem ${srcdir}" } */

/* Test that permerrors appear in system headers.  */

/* The dg-* directives in the header file are ignored.  No warnings are
   expected.  */
#include <gcc.dg/permerror-default.c>

/* These errors come from permerror-default.c.  */

/* { dg-error "'f1' \\\[-Wimplicit-function-declaration\\\]" "" { target *-*-* } 10 } */

/* { dg-error "'implicit_int_1' \\\[-Wimplicit-int\\\]" "" { target *-*-* } 13 } */
/* { dg-error "'implicit_int_2' \\\[-Wimplicit-int\\\]" "" { target *-*-* } 14 } */
/* { dg-error "'implicit_int_3' \\\[-Wimplicit-int\\]" "" { target *-*-* } 15 } */
/* { dg-error "return type defaults to 'int' \\\[-Wimplicit-int\\\]" "" { target *-*-* } 16 } */
/* { dg-error "type of 'i' defaults to 'int' \\\[-Wimplicit-int\\\]" "" { target *-*-*} 16 } */
/* { dg-error "type defaults to 'int' in type name \\\[-Wimplicit-int\\\]" "" { target *-*-* } 19 } */

/* { dg-error "parameter names \\\(without types\\\) in function declaration \\\[-Wdeclaration-missing-parameter-type\\\]" "" { target *-*-* } 22 } */

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

/* { dg-error "pointer type mismatch in conditional expression \\\[-Wincompatible-pointer-types\\\]" "" { target *-*-* } 55 } */
/* { dg-error "initialization of 'int \\\*' from pointer to '__builtin_abs' with incompatible type 'int \\\(\\\*\\\)\\\(int\\\)' \\\[-Wincompatible-pointer-types\\\]" "" { target *-*-* } 57 } */
/* { dg-error "assignment to 'int \\\*' from pointer to '__builtin_abs' with incompatible type 'int \\\(\\\*\\\)\\\(int\\\)' \\\[-Wincompatible-pointer-types\\\]" "" { target *-*-* } 58 } */
/* { dg-error "initialization of 'int \\\*' from incompatible pointer type 'int \\\* \\\(\\\*\\\)\\\(int\\\)' \\\[-Wincompatible-pointer-types\\\]" "" { target *-*-* } 61 } */
/* { dg-error "assignment to 'int \\\*' from incompatible pointer type 'int \\\* \\\(\\\*\\\)\\\(int\\\)' \\\[-Wincompatible-pointer-types\\\]" "" { target *-*-* } 62 } */
/* { dg-error "initialization of 'int \\\*' from incompatible pointer type 'int \\\*\\\*' \\\[-Wincompatible-pointer-types\\\]" "" { target *-*-* } 64 } */
/* { dg-error "assignment to 'int \\\*' from incompatible pointer type 'int \\\*\\\*' \\\[-Wincompatible-pointer-types\\\]" "" { target *-*-* } 65 } */
/* { dg-error "passing argument 1 of 'f4' from incompatible pointer type \\\[-Wincompatible-pointer-types\\\]" "" { target *-*-* } 67 } */
/* { dg-error "returning pointer to '__builtin_abs' of type 'int \\\(\\\*\\\)\\\(int\\\)' from a function with incompatible type 'int \\\*' \\\[-Wincompatible-pointer-types\\\]" "" { target *-*-* } 70 } */
/* { dg-error "returning 'int \\\* \\\(\\\*\\\)\\\(int\\\)' from a function with incompatible return type 'int \\\*' \\\[-Wincompatible-pointer-types\\\]" "" { target *-*-* } 72 } */

/* { dg-error "'return' with a value, in function returning void \\\[-Wreturn-mismatch\\\]" "" { target *-*-* } 78 } */
/* { dg-error "return' with no value, in function returning non-void \\\[-Wreturn-mismatch\\\]" "" { target *-*-* } 84 } */
