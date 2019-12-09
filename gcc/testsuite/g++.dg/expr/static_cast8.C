// PR c++/52321
struct A1; // { dg-message "note: class type 'A1' is incomplete" }
struct A2; // { dg-message "note: class type 'A2' is incomplete" }
struct B1; // { dg-message "note: class type 'B1' is incomplete" }
struct B2; // { dg-message "note: class type 'B2' is incomplete" }

struct C { };
extern C* c;

void pointers(C* c, A2* a2, B1* b1)
{
  (void) static_cast<A1*>(c);	// { dg-error "10:invalid 'static_cast'" }
  (void) static_cast<C*>(a2);	// { dg-error "10:invalid 'static_cast'" }
  (void) static_cast<B2*>(b1);	// { dg-error "10:invalid 'static_cast'" }
}

struct D1; // { dg-message "note: class type 'D1' is incomplete" }
struct D2; // { dg-message "note: class type 'D2' is incomplete" }
struct E1; // { dg-message "note: class type 'E1' is incomplete" }
struct E2; // { dg-message "note: class type 'E2' is incomplete" }

void references(C& c, D2& d2, E1& e1)
{
  (void) static_cast<D1&>(c);	// { dg-error "10:invalid 'static_cast'" }
  (void) static_cast<C&>(d2);	// { dg-error "10:invalid 'static_cast'" }
  (void) static_cast<E2&>(e1);	// { dg-error "10:invalid 'static_cast'" }
}
