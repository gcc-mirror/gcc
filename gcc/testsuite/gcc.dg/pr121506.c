/* PR c/121506 */
/* { dg-do compile } */

#include <stdarg.h>

struct A;
void foo (struct A *);	/* { dg-message "previous declaration of 'foo' with type 'void\\\(struct A \\\*\\\)'" } */
void foo (va_list);	/* { dg-error "conflicting types for 'foo'; have" } */
