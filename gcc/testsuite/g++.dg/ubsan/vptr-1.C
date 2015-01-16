// { dg-do run { target { ilp32 || lp64 } } }
// { dg-options "-fsanitize=vptr" }

struct S
{
  S() : a(0) {}
  ~S() {}
  int a;
  int f() { return 0; }
  virtual int v() { return 0; }
};

struct T : S
{
  T() : b(0) {}
  int b;
  int g() { return 0; }
  virtual int v() { return 1; }
};

struct U : S, T { virtual int v() { return 2; } }; // { dg-warning "direct base .S. inaccessible in .U. due to ambiguity" }
struct V : S {};

void
foo ()
{
  T t;
  (void)t.a;
  (void)t.b;
  (void)t.f();
  (void)t.g();
  (void)t.v();
  (void)t.S::v();

  U u;
  (void)u.T::a;
  (void)u.b;
  (void)u.T::f();
  (void)u.g();
  (void)u.v();
  (void)u.T::v();
  (void)((T&)u).S::v();
}

T *x;

__attribute__((noinline, noclone)) int
bar (T *p, int q)
{
  switch (q)
    {
    // These shouldn't fail:
    case 0x10:
    case 0x20:
    case 0x30:
    case 0x40:
      {
	T &r = *p;
	break;
      }
    case 0x21:
    case 0x31:
      return p->b;
    case 0x22:
    case 0x32:
      return p->g ();
    case 0x23:
    case 0x33:
      x = static_cast<T*>(reinterpret_cast<S*>(p));
      break;
    case 0x44:
      return reinterpret_cast<U*>(p)->v() - 2;
    // These should:
    case 0x11:
      return p->b;
    // { dg-output "\[^\n\r]*vptr-1.C:75:\[0-9]*: runtime error: member access within address 0x\[0-9a-fA-F]* which does not point to an object of type 'T'(\n|\r\n|\r)" }
    // { dg-output "0x\[0-9a-fA-F]*: note: object is of type 'S'(\n|\r\n|\r)" }
    // { dg-output " .. .. .. ..  .. .. .. .. .. .. .. ..  \[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              \\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              vptr for 'S'\[^\n\r]*(\n|\r\n|\r)" }
    case 0x12:
      return p->g ();
    // { dg-output "\[^\n\r]*vptr-1.C:82:\[0-9]*: runtime error: member call on address 0x\[0-9a-fA-F]* which does not point to an object of type 'T'(\n|\r\n|\r)" }
    // { dg-output "0x\[0-9a-fA-F]*: note: object is of type 'S'(\n|\r\n|\r)" }
    // { dg-output " .. .. .. ..  .. .. .. .. .. .. .. ..  \[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              \\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              vptr for 'S'\[^\n\r]*(\n|\r\n|\r)" }
    case 0x13:
      x = static_cast<T*>(reinterpret_cast<S*>(p));
      break;
    // { dg-output "\[^\n\r]*vptr-1.C:89:\[0-9]*: runtime error: downcast of address 0x\[0-9a-fA-F]* which does not point to an object of type 'T'(\n|\r\n|\r)" }
    // { dg-output "0x\[0-9a-fA-F]*: note: object is of type 'S'(\n|\r\n|\r)" }
    // { dg-output " .. .. .. ..  .. .. .. .. .. .. .. ..  \[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              \\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              vptr for 'S'\[^\n\r]*(\n|\r\n|\r)" }
    case 0x34:
      return reinterpret_cast<U*>(p)->v() - 2;
    // { dg-output "\[^\n\r]*vptr-1.C:97:\[0-9]*: runtime error: member call on address 0x\[0-9a-fA-F]* which does not point to an object of type 'U'(\n|\r\n|\r)" }
    // { dg-output "0x\[0-9a-fA-F]*: note: object is base class subobject at offset 16 within object of type 'U'(\n|\r\n|\r)" { target lp64 } }
    // { dg-output "0x\[0-9a-fA-F]*: note: object is base class subobject at offset 8 within object of type 'U'(\n|\r\n|\r)" { target ilp32 } }
    // { dg-output " .. .. .. ..  .. .. .. .. .. .. .. ..  .. .. .. .. .. .. .. ..  .. .. .. .. .. .. .. ..  \[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              \\^                                                 ~~~~~~~~~~~~~~~~~~~~~~~(\n|\r\n|\r)" { target lp64 } }
    // { dg-output "                                                                vptr for 'T' base class of 'U'\[^\n\r]*(\n|\r\n|\r)" { target lp64 } }
    // { dg-output "              \\^                        ~~~~~~~~~~~(\n|\r\n|\r)" { target ilp32 } }
    // { dg-output "                                       vptr for 'T' base class of 'U'\[^\n\r]*(\n|\r\n|\r)" { target ilp32 } }
    case 0x41:
      return p->b;
    // { dg-output "\[^\n\r]*vptr-1.C:107:\[0-9]*: runtime error: member access within address 0x\[0-9a-fA-F]* which does not point to an object of type 'T'(\n|\r\n|\r)" }
    // { dg-output "0x\[0-9a-fA-F]*: note: object is of type 'U'(\n|\r\n|\r)" }
    // { dg-output " .. .. .. ..  .. .. .. .. .. .. .. ..  \[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              \\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              vptr for 'U'\[^\n\r]*(\n|\r\n|\r)" }
    case 0x42:
      return p->g ();
    // { dg-output "\[^\n\r]*vptr-1.C:114:\[0-9]*: runtime error: member call on address 0x\[0-9a-fA-F]* which does not point to an object of type 'T'(\n|\r\n|\r)" }
    // { dg-output "0x\[0-9a-fA-F]*: note: object is of type 'U'(\n|\r\n|\r)" }
    // { dg-output " .. .. .. ..  .. .. .. .. .. .. .. ..  \[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              \\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              vptr for 'U'\[^\n\r]*(\n|\r\n|\r)" }
    case 0x43:
      x = static_cast<T*>(reinterpret_cast<S*>(p));
      break;
    // { dg-output "\[^\n\r]*vptr-1.C:121:\[0-9]*: runtime error: downcast of address 0x\[0-9a-fA-F]* which does not point to an object of type 'T'(\n|\r\n|\r)" }
    // { dg-output "0x\[0-9a-fA-F]*: note: object is of type 'U'(\n|\r\n|\r)" }
    // { dg-output " .. .. .. ..  .. .. .. .. .. .. .. ..  \[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              \\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
    // { dg-output "              vptr for 'U'\[^\n\r]*(\n|\r\n|\r)" }
    case 0x51:
      return p->b;
    // { dg-output "\[^\n\r]*vptr-1.C:129:\[0-9]*: runtime error: member access within address 0x\[0-9a-fA-F]* which does not point to an object of type 'T'(\n|\r\n|\r)" }
    // { dg-output "0x\[0-9a-fA-F]*: note: object has invalid vptr(\n|\r\n|\r)" }
    // { dg-output " .. .. .. ..  00 00 00 00 00 00 00 00  \[^\n\r]*(\n|\r\n|\r)" { target lp64 } }
    // { dg-output "              \\^~~~~~~~~~~~~~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" { target lp64 } }
    // { dg-output "  ?.. .. .. ..  ?00 00 00 00  ?.. .. .. ..  ?\[^\n\r]*(\n|\r\n|\r)" { target ilp32 } }
    // { dg-output "              \\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" { target ilp32 } }
    // { dg-output "              invalid vptr" }
    }
  return 0;
}

char b[sizeof (U)] __attribute__((aligned (__alignof__ (U)))) = {};

__attribute__((noinline, noclone)) void
baz (int q)
{
  T *p = 0;
  S *s = 0;
  U *u = 0;
  switch (q)
    {
    case 0x10: case 0x11: case 0x12: case 0x13:
      s = new S;
      bar (reinterpret_cast<T *>(s), q);
      delete s;
      break;
    case 0x20: case 0x21: case 0x22: case 0x23:
      p = new T;
      bar (p, q);
      delete p;
      break;
    case 0x30: case 0x31: case 0x32: case 0x33: case 0x34:
      u = new U;
      bar (u, q);
      delete u;
      break;
    case 0x40: case 0x41: case 0x42: case 0x43: case 0x44:
      u = new U;
      bar (reinterpret_cast<T *>(u), q);
      delete u;
      break;
    case 0x51:
      p = reinterpret_cast<T*>(b);
      bar (p, q);
      break;
    }
}

int
main ()
{
  foo ();
  for (int q = 0; q < 0x52; q++)
    baz (q);
}
