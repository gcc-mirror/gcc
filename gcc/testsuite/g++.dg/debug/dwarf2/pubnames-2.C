// { dg-do compile { target c++11 } }
// { dg-skip-if "" { powerpc-ibm-aix* } }
// { dg-options "-gpubnames -gdwarf-4 -fno-debug-types-section -dA" }
// { dg-final { scan-assembler-times "\.section\[\t \]\[^\n\]*debug_pubnames" 1 } }
// { dg-final { scan-assembler "\"\\(anonymous namespace\\)\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one::G_A\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one::G_B\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one::G_C\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one::\\(anonymous namespace\\)\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"F_A\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"F_B\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"F_C\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"inline_func_1\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one::c1::c1\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one::c1::~c1\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one::c1::val\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"check_enum\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"main\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<int>::c2\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<double>::c2\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<int const\\\*>::c2\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"check<one::c1>\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"check<two::c2<int> \\>\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"check<two::c2<double> \\>\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"check<two::c2<int const\\\*> \\>\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<int>::val\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<double>::val\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<int const\\\*>::val\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"__static_initialization_and_destruction_0\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<int>::~c2\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<double>::~c2\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<int const\\\*>::~c2\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"anonymous_union_var\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::ci\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2v1\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2v2\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2v3\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one::c1v\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one::\\(anonymous namespace\\)::one_anonymous_var\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"\\(anonymous namespace\\)::c1_count\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"\\(anonymous namespace\\)::c2_count\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"\\(anonymous namespace\\)::three\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"\\(anonymous namespace\\)::three::anonymous_three_var\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler-times "\.section\[\t \]\[^\n\]*debug_pubtypes" 1 } }
// { dg-final { scan-assembler "\"one::G\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one::c1\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"int\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"one::c1\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<int>\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<int>\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<double>\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"double\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<double>\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<int const\\\*>\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"two::c2<int const\\\*>\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"F\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"anonymous_union_container\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }
// { dg-final { scan-assembler "\"bool\\\\0\"+\[ \t\]+\[#;/|@!]+\[ \t\]+external name" } }

namespace
{
int c1_count;
int c2_count;

namespace three
{
int anonymous_three_var;
}
};

namespace one
{

enum G
{
  G_A,
  G_B,
  G_C
};

namespace {
int one_anonymous_var;
}

class c1
{
 public:
  static int count;

  c1()
  { ++c1_count; }

  ~c1()
  {
    --c1_count;
  }

  enum E
  {
    E_A,
    E_B,
    E_C,
  };

  int
  val()
  { return E_A; }
};

c1 c1v;
};

namespace two
{
const int ci = 3;

template <typename T>
class c2
{
 public:
  c2(T t)
    : t_(t)
  {
    ++c2_count;
  }

  ~c2()
  { --c2_count; }

  T
  val()
  { return this->t_; }

  T t_;
};

c2<int> c2v1(1);
c2<double> c2v2(2.0);
c2<int const*> c2v3(&ci);
};

enum F
{
  F_A,
  F_B,
  F_C
};

template <class C>
bool
check(C* c)
{ return c->val() == 0; }

bool
check_enum(int i)
{ return i > 0; }

struct anonymous_union_container {
  union {
    struct astruct {
      int a;
    };
    int b;
  } u;
};

anonymous_union_container anonymous_union_var;

#ifdef __GNUC__
#define ALWAYS_INLINE __attribute__((always_inline))
#else
#define ALWAYS_INLINE
#endif

static inline ALWAYS_INLINE int
inline_func_1(int i)
{ return i * 17; }

int
main()
{
  F f = F_A;
  one::G g = one::G_A;
  check_enum(f);
  check_enum(g);
  check(&one::c1v);
  check(&two::c2v1);
  check(&two::c2v2);
  check(&two::c2v3);
  anonymous_union_var.u.b = inline_func_1(3) - 51;
  return anonymous_union_var.u.b;
}
