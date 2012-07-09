/* PR 30457 warn about va_start(ap, invalid) */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */

/* Undefined by C99 7.15.1.4p4 (va_start):
   "If the parameter parmN is declared with the register storage
    class, with a function or array type, or with a type that is
    not compatible with the type that results after application of
    the default argument promotions, the behavior is undefined."  */

#include <stdarg.h>

void foo(register short paramN, ...)
{
  va_list ap;

  va_start(ap, paramN); /* { dg-warning "undefined behaviour when second parameter of 'va_start' is declared with 'register' storage" } */
  
  /* Undefined by C99 7.15.1.1p2:  */
  (void) va_arg(ap, char); /* { dg-warning "'char' is promoted to 'int' when passed through '...'" "promoted" } */
  /* { dg-message "note: .so you should pass .int. not .char. to .va_arg.." "int not char" { target *-*-* } 20 } */
  /* { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } 20 } */

  va_end(ap);
}

