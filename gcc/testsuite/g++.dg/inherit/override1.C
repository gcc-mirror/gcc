// { dg-options "-w" }

struct c0 { virtual void f (); };
struct c1 : public c0 {};
struct c2 : public c0 {};
struct c3 : virtual public c0, public c1, public c2 {};
struct c5 {};
struct c7 : virtual public c3 {};
struct c8 : virtual public c1 { virtual void f (); };
struct c9 {};
struct c10 : virtual public c8, virtual public c7 {};
struct c11 : virtual public c5 {};
struct c12 : virtual public c8, public c7 {};
struct c13 : public c9, public c3, virtual public c2 {};
struct c14 : virtual public c1, virtual public c5, virtual public c0,
	     public c2 {};
struct c15 : public c14, public c12, virtual public c3 {};
struct c16 : public c12, public c10, public c2 { virtual void f (); };
struct c17 : virtual public c13, public c15, virtual public c0,
	     virtual public c16 {};
