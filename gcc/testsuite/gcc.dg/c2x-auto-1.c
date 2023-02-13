/* Test C2x auto.  Valid code, compilation tests.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

auto i = 1;
extern int i;
static auto l = 0L;
extern long l;
extern auto const d = 0.0; /* { dg-warning "initialized and declared 'extern'" } */
extern const double d;
double dx;
auto ((i2)) = 3;
extern int i2;
const auto i3 [[]] = 4;
extern int i4;
thread_local auto f = 1.0f;
float ff;
extern typeof (f) ff;
auto h = (short) 0;
extern short h;

struct s { int a; };
struct s sv;
struct s2;
enum e : int;

extern const volatile long double cvld;
extern void (*tfp) (void);

int a[10];
int *ap;

typedef int ti;

void
tf ()
{
  auto asv = (struct s) { 0 };
  extern typeof (asv) sv;
  auto s2p = (struct s2 *) 0;
  struct s3;
  auto s3p = (struct s3 *) 0;
  auto ev = (enum e) 0;
  static const auto volatile acvld = 0.5L;
  extern typeof (acvld) cvld;
  /* lvalue conversion occurs on the initializer, so losing qualifiers.  */
  auto ncd = d;
  extern typeof (ncd) dx;
  _Atomic double ad = 0.0;
  auto nad = ad;
  extern typeof (nad) dx;
  /* Function-to-pointer conversion occurs on the initializer.  */
  auto fp = tf;
  extern typeof (fp) tfp;
  /* Array-to-pointer conversion occurs on the initializer.  */
  auto aap = a;
  extern typeof (aap) ap;
  /* Shadowing a declaration from a containing scope is OK.  */
  auto i = 2L;
  extern typeof (i) l;
  /* auto can be used in for loops.  */
  for (auto ix = 2; ix < 10; ix++)
    {
      extern typeof (ix) i2;
    }
  /* auto is valid with bit-field initializers; the choice of type those have
     in expressions is unspecified but should match how _Generic handles such
     expressions.  */
  struct b { int a : 2; unsigned b : 3; } bv = { };
  auto bfa = bv.a;
  auto bfb = bv.b;
  static_assert (_Generic (bv.a, typeof (bfa) : 1, default : 2) == 1);
  static_assert (_Generic (bv.b, typeof (bfb) : 1, default : 2) == 1);
  /* The traditional meaning of auto with a type specifier is OK.  */
  auto short s;
  char auto c;
  auto struct t { int x; } t;
  /* That includes the case where the type comes from a typedef name.  */
  auto ti int_from_typedef = 3.0;
  extern typeof (int_from_typedef) i2;
}
