/* K+R rejects use of function-like macros in non-function context.
   ANSI C explicitly permits this (the macro is not expanded).  */

/* { dg-do compile } */
/* { dg-options -Wtraditional } */

enum { SIGN_EXTEND = 23 };

#define SIGN_EXTEND(v) (((v) < 0) ? -1 : 0)

int fun(void)
{
  return SIGN_EXTEND;	/* { dg-warning "must be used with arguments" } */
}
