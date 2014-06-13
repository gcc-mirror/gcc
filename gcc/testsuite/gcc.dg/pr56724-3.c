/* PR c/56724 */
/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

extern void xfer (int, int, unsigned char *);
struct T { int a; } t;

void
call (int x, int y, void *arg)
{
  unsigned char *uc = arg; /* { dg-warning "23:request for implicit conversion" } */
  xfer (x, y, arg); /* { dg-warning "15:request for implicit conversion" } */
  xfer (x, y, t); /* { dg-error "15:incompatible type for" } */
}
