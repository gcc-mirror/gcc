// P3074R7 - trivial unions (was std::uninitialized<T>)
// P3726R2 - Adjustments to Union Lifetime Rules
// { dg-do compile { target c++20 } }

struct A { int a[2]; };
union B { int a; int b[2]; };
union C { int a; int b[2][2]; };
struct D { B a; };
struct E { C a; };
union F { int a; A b; };
union G { int a; A b[2]; };

constexpr A fA () { A a; a.a[0] = 1; return a; }
constexpr A a = fA ();		// { dg-error "is not a constant expression" "" { xfail *-*-* } }
constexpr B fB () { B a; a.b[0] = 1; return a; }
constexpr B b = fB ();		// { dg-error "is not a constant expression" }
constexpr C fC () { C a; a.b[0][0] = 1; a.b[1][0] = 2; a.b[1][1] = 3; return a; }
constexpr C c = fC ();		// { dg-error "is not a constant expression" }
constexpr D fD () { D a; a.a.b[0] = 1; return a; }
constexpr D d = fD ();		// { dg-error "is not a constant expression" }
constexpr E fE () { E a; a.a.b[0][0] = 1; a.a.b[1][0] = 2; a.a.b[1][1] = 3; return a; }
constexpr E e = fE ();		// { dg-error "is not a constant expression" }
constexpr F fF () { F a; a.b.a[0] = 1; return a; }
constexpr F f = fF ();		// { dg-error "is not a constant expression" }
constexpr G fG () { G a; a.b[0].a[0] = 1; return a; }
constexpr G g = fG ();		// { dg-error "is not a constant expression" }
constexpr B
fB2 ()
{
  B a;
  __builtin_start_lifetime (&a.b);
  a.b[0] = 1;
  return a;
}
constexpr B b2 = fB2 ();	// { dg-error "is not a constant expression" "" { target c++23_down } }
constexpr C
fC2 (bool x)
{
  C a;
  __builtin_start_lifetime (&a.b);
  __builtin_start_lifetime (&a.b[0]);
  if (x)
    __builtin_start_lifetime (&a.b[1]);
  a.b[0][0] = 1;
  a.b[1][0] = 2;
  a.b[1][1] = 3;
  return a;
}
constexpr C c2 = fC2 (false);	// { dg-error "is not a constant expression" "" { target c++23_down } }
constexpr C c3 = fC2 (true);	// { dg-error "is not a constant expression" "" { target c++23_down } }
constexpr D
fD2 ()
{
  D a;
  __builtin_start_lifetime (&a.a.b);
  a.a.b[0] = 1;
  return a;
}
constexpr D d2 = fD2 ();	// { dg-error "is not a constant expression" "" { target c++23_down } }
constexpr E
fE2 (bool x)
{
  E a;
  __builtin_start_lifetime (&a.a.b);
  __builtin_start_lifetime (&a.a.b[0]);
  if (x)
    __builtin_start_lifetime (&a.a.b[1]);
  a.a.b[0][0] = 1;
  a.a.b[1][0] = 2;
  a.a.b[1][1] = 3;
  return a;
}
constexpr E e2 = fE2 (false);	// { dg-error "is not a constant expression" "" { target c++23_down } }
constexpr E e3 = fE2 (true);	// { dg-error "is not a constant expression" "" { target c++23_down } }
constexpr E
fE3 (bool x)
{
  E a;
  __builtin_start_lifetime (&a.a.b[0]);	// { dg-error "'__builtin_start_lifetime' with containing object not within lifetime" }
  if (x)
    __builtin_start_lifetime (&a.a.b[1]);
  a.a.b[0][0] = 1;
  a.a.b[1][1] = 3;
  return a;
}
constexpr E e4 = fE3 (false);
constexpr E e5 = fE3 (true);
union H { int a; A b[2][2]; };
union I { int a; H b[2]; };
constexpr bool
fH (int x)
{
  G a;
  I b;
  switch (x)
    {
    case 0:
      __builtin_start_lifetime (&a.b);
      break;
    case 1:
      __builtin_start_lifetime (&a.b[0]);	// { dg-error "'__builtin_start_lifetime' with containing object not within lifetime" }
      break;
    case 2:
      a.a = 42;
      __builtin_start_lifetime (&a.b);
      break;
    case 3:
      a.a = 42;
      __builtin_start_lifetime (&a.b[1]);	// { dg-error "'__builtin_start_lifetime' with containing object not within lifetime" }
      break;
    case 4:
      __builtin_start_lifetime (&b.b);
      break;
    case 5:
      __builtin_start_lifetime (&b.b[0]);	// { dg-error "'__builtin_start_lifetime' with containing object not within lifetime" }
      break;
    case 6:
      a.a = 42;
      __builtin_start_lifetime (&b.b);
      break;
    case 7:
      a.a = 42;
      __builtin_start_lifetime (&b.b[1]);	// { dg-error "'__builtin_start_lifetime' with containing object not within lifetime" }
      break;
    case 8:
      __builtin_start_lifetime (&b.b[1].b[0][1]); // { dg-error "'__builtin_start_lifetime' with containing object not within lifetime" }
      break;
    case 9:
      __builtin_start_lifetime (&b.b);
      __builtin_start_lifetime (&b.b[1]);
      __builtin_start_lifetime (&b.b[1].b);
      __builtin_start_lifetime (&b.b[1].b[0]);
      __builtin_start_lifetime (&b.b[1].b[0][1]);
      break;
    case 10:
      __builtin_start_lifetime (&b.b);
      __builtin_start_lifetime (&b.b[1]);
      __builtin_start_lifetime (&b.b[1].b);
      __builtin_start_lifetime (&b.b[1].b[0][1]); // { dg-error "'__builtin_start_lifetime' with containing object not within lifetime" }
      break;
    case 11:
      __builtin_start_lifetime (&b.b);
      __builtin_start_lifetime (&b.b[1]);
      __builtin_start_lifetime (&b.b[1].b[0]); // { dg-error "'__builtin_start_lifetime' with containing object not within lifetime" }
      break;
    case 12:
      __builtin_start_lifetime (&b.b);
      __builtin_start_lifetime (&b.b[1].b); // { dg-error "'__builtin_start_lifetime' with containing object not within lifetime" }
      break;
    default:
      break;
    }
  return true;
}
static_assert (fH (0));
constexpr bool h1 = fH (1);
static_assert (fH (2));
constexpr bool h3 = fH (3);
static_assert (fH (4));
constexpr bool h5 = fH (5);
static_assert (fH (6));
constexpr bool h7 = fH (7);
constexpr bool h8 = fH (8);
static_assert (fH (9));
constexpr bool h10 = fH (10);
constexpr bool h11 = fH (11);
constexpr bool h12 = fH (12);
