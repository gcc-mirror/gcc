/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

#define not_octal_constant 0.627

extern double foo (double);

/* { dg-final { scan-ada-spec-not "unsupported macro" } } */
/* { dg-final { cleanup-ada-spec } } */
