/* { dg-do compile } */

extern int f() __attribute__((returns_nonnull)); /* { dg-error "not returning a pointer" } */
