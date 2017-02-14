/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wall -Wextra -Wpedantic" } */

extern void bar (int);
void
fn (int i)
{
  __attribute__((fallthrough)) int j = 0; /* { dg-warning "ignored|attribute not followed" } */

  if (j)
    __attribute__((fallthrough));  /* { dg-error "invalid use" } */

  __attribute__((fallthrough));  /* { dg-error "invalid use" } */
  switch (i)
  {
    __attribute__((fallthrough)); /* { dg-warning "statement will never" } */
  case 1:
   i++;
   __attribute__((fallthrough));
  case 2:
    if (i) /* { dg-warning "statement may fall through" } */
      bar (2);
    else
      __attribute__((fallthrough));
  case 3:
    if (i > 1)
      __attribute__((fallthrough));
    else
      return;
  case 4:
    if (i)
      __attribute__((fallthrough)); /* { dg-warning "not preceding" } */
    __attribute__((fallthrough));
  case 5:
   ;
   __attribute__((fallthrough));
  case 6:
    if (i) /* { dg-warning "statement may fall through" } */
      bar (6);
    else
      {
	__attribute__((fallthrough));
      }
  case 7:
    if (i > 1)
      {
	__attribute__((fallthrough));
      }
    else
      bar (7); /* { dg-warning "statement may fall through" } */
  default:
    --j;
  }

  __attribute__((fallthrough)); /* { dg-error "invalid use" } */
}
