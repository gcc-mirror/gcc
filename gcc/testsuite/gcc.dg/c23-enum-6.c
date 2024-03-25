/* Test C23 enumerations with fixed underlying type.  Valid code.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

/* Check a type while defining an enum (via a diagnostic for incompatible
   pointer types if the wrong type was chosen).  */
#define TYPE_CHECK(cst, type)						\
  cst ## _type_check = sizeof (1 ? (type *) 0 : (typeof (cst) *) 0)

extern int i;

enum e1 : short { e1a = __SHRT_MAX__,
    TYPE_CHECK (e1a, short),
    e1z = (long long) 0,
    TYPE_CHECK (e1z, enum e1),
    e1b = -__SHRT_MAX__ - 1,
    e1c,
    TYPE_CHECK (e1c, enum e1) };
extern enum e1 e1v;
extern typeof (e1a) e1v;
extern typeof (e1b) e1v;
extern typeof (e1c) e1v;
extern typeof (e1z) e1v;
extern short e1v;
static_assert (e1a == __SHRT_MAX__);
static_assert (e1b == -__SHRT_MAX__ - 1);
static_assert (e1c == -__SHRT_MAX__);
static_assert (e1a > 0);
static_assert (e1b < 0);
static_assert (e1c < 0);
static_assert (e1z == 0);
extern typeof (+e1v) i;
extern typeof (+e1a) i;
extern typeof (e1a + e1b) i;
enum e1 : short;
enum e1 : volatile short;
enum e1 : _Atomic short;
enum e1 : typeof (short);

enum e2 : bool { b0, b1, b0a = 0, b1a = 1 };
extern enum e2 e2v;
extern typeof (b0) e2v;
extern typeof (b0a) e2v;
extern typeof (b1) e2v;
extern typeof (b1a) e2v;
extern bool e2v;
extern typeof (+e2v) i;
extern typeof (+b0) i;
static_assert (b0 == 0);
static_assert (b1 == 1);
static_assert (b0a == 0);
static_assert (b1a == 1);

enum e3 : volatile const _Atomic unsigned short;
enum e3 : unsigned short { e3a, e3b };
extern enum e3 e3v;
extern typeof (e3a) e3v;
extern typeof (e3b) e3v;
extern unsigned short e3v;

/* The enum type is complete from the end of the first enum type specifier
   (which is nested inside another enum type specifier in this example).  */
enum e4 : typeof ((enum e4 : long { e4a = sizeof (enum e4) })0, 0L);
extern enum e4 e4v;
extern typeof (e4a) e4v;
extern long e4v;

enum e5 : unsigned int;
extern enum e5 e5v;
extern typeof (e5v + e5v) e5v;
extern unsigned int e5v;

enum : unsigned short { e6a, e6b, TYPE_CHECK (e6a, unsigned short) } e6v;
extern typeof (e6a) e6v;
extern typeof (e6b) e6v;
extern unsigned short e6v;

struct s1;
struct s2 { int a; };
union u1;
union u2 { int a; };
enum xe1 { XE1 };
enum xe2 : long long { XE2 };
enum xe3 : unsigned long;

void
f ()
{
  /* Tags can be redeclared in an inner scope.  */
  enum s1 : char;
  enum s2 : int { S2 };
  enum u1 : long { U1 };
  enum u2 : unsigned char;
  enum xe1 : long long;
  enum xe2 : short;
  enum xe3 : char { XE3 };
  static_assert (sizeof (enum xe3) == 1);
  static_assert (sizeof (enum xe2) == sizeof (short));
  static_assert (sizeof (enum xe1) == sizeof (long long));
}

void *p;
typeof (nullptr) np;

extern void abort (void);
extern void exit (int);

int
main ()
{
  /* Conversions to enums with fixed underlying type have the same semantics as
     converting to the underlying type.  */
  volatile enum e1 e1vm;
  volatile enum e2 e2vm;
  e1vm = __LONG_LONG_MAX__; /* { dg-warning "overflow" } */
  if (e1vm != (short) __LONG_LONG_MAX__)
    abort ();
  e2vm = 10;
  if (e2vm != 1)
    abort ();
  e2vm = 0;
  if (e2vm != 0)
    abort ();
  /* Arithmetic on enums with fixed underlying type has the same semantics as
     arithmetic on the underlying type; in particular, the special semantics
     for bool apply to enums with bool as fixed underlying type.  */
  if (e2vm++ != 0)
    abort ();
  if (e2vm != 1)
    abort ();
  if (e2vm++ != 1)
    abort ();
  if (e2vm != 1)
    abort ();
  if (e2vm-- != 1)
    abort ();
  if (e2vm != 0)
    abort ();
  if (e2vm-- != 0)
    abort ();
  if (e2vm != 1)
    abort ();
  if (++e2vm != 1)
    abort ();
  if (e2vm != 1)
    abort ();
  e2vm = 0;
  if (++e2vm != 1)
    abort ();
  if (e2vm != 1)
    abort ();
  if (--e2vm != 0)
    abort ();
  if (e2vm != 0)
    abort ();
  if (--e2vm != 1)
    abort ();
  if (e2vm != 1)
    abort ();
  e2vm = p;
  e2vm = np;
  e2vm = (bool) p;
  e2vm = (bool) np;
  if (e2vm != 0)
    abort ();
  exit (0);
}
