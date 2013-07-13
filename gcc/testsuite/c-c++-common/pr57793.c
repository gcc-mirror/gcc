/* PR c++/57793 */

struct A { unsigned a : 1; unsigned b : 1; };
struct B     /* { dg-error "type .B. is too large" "" { target { c++ && ilp32 } } } */
{
  unsigned char c[0x40000000];
  unsigned char d[0x40000ff0];
  struct A e;
}; /* { dg-error "type .struct B. is too large" "" { target { c && ilp32 } } } */

void *foo (struct B *p)
{
  if (p->e.a)
    return (void *) 0;
  p->e.b = 1;
  return p->c;
}

void
bar (struct B *p)
{
  foo (p);
}
