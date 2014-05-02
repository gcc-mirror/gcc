/* PR c/25801 */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */

int (*a)[];
struct S *s;
union U *u;
enum E *e;

void
f (void)
{
  a++; /* { dg-error "increment of pointer to an incomplete type" } */
  ++a; /* { dg-error "increment of pointer to an incomplete type" } */
  a--; /* { dg-error "decrement of pointer to an incomplete type" } */
  --a; /* { dg-error "decrement of pointer to an incomplete type" } */
  a += 1; /* { dg-error "invalid use of array with unspecified bounds" } */
  a -= 1; /* { dg-error "invalid use of array with unspecified bounds" } */
  a - a; /* { dg-error "arithmetic on pointer to an incomplete type" } */

  s++; /* { dg-error "increment of pointer to an incomplete type" } */
  ++s; /* { dg-error "increment of pointer to an incomplete type" } */
  s--; /* { dg-error "decrement of pointer to an incomplete type" } */
  --s; /* { dg-error "decrement of pointer to an incomplete type" } */
  s += 1; /* { dg-error "invalid use of undefined type" } */
  s -= 1; /* { dg-error "invalid use of undefined type" } */
  s - s; /* { dg-error "arithmetic on pointer to an incomplete type" } */

  u++; /* { dg-error "increment of pointer to an incomplete type" } */
  ++u; /* { dg-error "increment of pointer to an incomplete type" } */
  u--; /* { dg-error "decrement of pointer to an incomplete type" } */
  --u; /* { dg-error "decrement of pointer to an incomplete type" } */
  u += 1; /* { dg-error "invalid use of undefined type" } */
  u -= 1; /* { dg-error "invalid use of undefined type" } */
  u - u; /* { dg-error "arithmetic on pointer to an incomplete type" } */

  e++; /* { dg-error "increment of pointer to an incomplete type" } */
  ++e; /* { dg-error "increment of pointer to an incomplete type" } */
  e--; /* { dg-error "decrement of pointer to an incomplete type" } */
  --e; /* { dg-error "decrement of pointer to an incomplete type" } */
  e += 1; /* { dg-error "invalid use of undefined type" } */
  e -= 1; /* { dg-error "invalid use of undefined type" } */
  e - e; /* { dg-error "arithmetic on pointer to an incomplete type" } */
}
