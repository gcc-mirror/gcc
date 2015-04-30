/* PR c/65901 */
/* { dg-do compile } */
/* { dg-options "" } */

struct S;
enum E;
union U;

void
foo (__builtin_va_list ap)
{
  __builtin_va_arg (ap, void);  /* { dg-error "second argument to .va_arg. is of incomplete type .void." } */
  __builtin_va_arg (ap, struct S);  /* { dg-error "second argument to .va_arg. is of incomplete type .struct S." } */
  __builtin_va_arg (ap, enum E);  /* { dg-error "second argument to .va_arg. is of incomplete type .enum E." } */
  __builtin_va_arg (ap, union U);  /* { dg-error "second argument to .va_arg. is of incomplete type .union U." } */
}
