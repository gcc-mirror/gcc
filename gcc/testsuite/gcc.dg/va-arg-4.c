/* { dg-do compile } */
#include <stdarg.h>
extern void baz(...);	/* { dg-error "requires a named argument" } */
