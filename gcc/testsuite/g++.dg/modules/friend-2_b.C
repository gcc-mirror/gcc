// { dg-additional-options -fmodules-ts }

module bink;

struct other {};

void f (pusher *p, other *q)
{
  grabber (p);

  frob (p); // ok, found by ADL
  
  frob (q); // { dg-error "not declared" }
}
