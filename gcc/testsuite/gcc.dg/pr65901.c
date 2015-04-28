/* PR c/65901 */
/* { dg-do compile } */
/* { dg-options "" } */

struct S;
enum E;
union U;

void
foo (__builtin_va_list ap)
{
  __builtin_va_arg (ap, void);  /* { dg-error "invalid use of void expression" } */
  __builtin_va_arg (ap, struct S);  /* { dg-error "invalid use of undefined type" } */
  __builtin_va_arg (ap, enum E);  /* { dg-error "invalid use of undefined type" } */
  __builtin_va_arg (ap, union U);  /* { dg-error "invalid use of undefined type" } */
}
