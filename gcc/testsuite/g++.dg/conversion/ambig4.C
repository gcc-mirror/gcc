// PR c++/92705
// { dg-do compile }

struct A {};
struct B {};
struct C { operator B * (); };	// { dg-message "candidate" }
struct D { operator B * (); };	// { dg-message "candidate" }
struct E : C, D { operator A * (); };

void
foo (E e, int B::* pmf)
{
  int i = e->*pmf;	// { dg-error "is ambiguous" }
}
