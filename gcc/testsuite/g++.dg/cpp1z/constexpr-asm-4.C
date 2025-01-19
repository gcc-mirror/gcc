// PR c++/118277
// { dg-do compile { target c++14 } }
// { dg-options "" }

using size_t = decltype (sizeof (0));
struct string_view {
  size_t s;
  const char *d;
  constexpr string_view () : s (0), d (nullptr) {}
  constexpr string_view (const char *p) : s (__builtin_strlen (p)), d (p) {}
  constexpr string_view (size_t l, const char *p) : s (l), d (p) {}
  constexpr size_t size () const noexcept { return s; }
  constexpr const char *data () const noexcept { return d; }
};

template <typename T>
constexpr T
gen (int n)
{
  switch (n)
    {
    case 0: return "foo %3,%2,%1,%0";
    case 1: return "=r";
    case 2: return "r";
    case 3: return "memory";
    case 4: return "cc";
    case 5: return "goo %3,%2,%1,%0";
    case 6: return "hoo %3,%2,%1,%0";
    case 7: return "ioo";
    case 8: return "joo";
    case 9: return "koo";
    default: return "";
    }
}

int
bar ()
{
  int a, b;
  asm ((gen <string_view> (0))
       : (gen <string_view> (1)) (a), (gen <string_view> (1)) (b)
       : (gen <string_view> (2)) (1), (gen <string_view> (2)) (2)
       : (gen <string_view> (3)), (gen <string_view> (4)));
  asm ((gen <string_view> (7)));
  return a + b;
}

template <typename T, typename U>
U
baz ()
{
  U a, b;
  asm ((gen <T> (5))
       : (gen <T> (1)) (a), (gen <T> (1)) (b)
       : (gen <T> (2)) (U(1)), (gen <T> (2)) (U(2))
       : (gen <T> (3)), (gen <T> (4)));
  asm ((gen <string_view> (8)));
  return a + b;
}

template <typename T, typename U>
U
qux ()
{
  U a, b;
  asm ((gen <T> (6))
       : (gen <T> (1)) (a), (gen <T> (1)) (b)
       : (gen <T> (2)) (U(1)), (gen <T> (2)) (U(2))
       : (gen <T> (3)), (gen <T> (4)));
  asm ((gen <string_view> (9)));
  return a + b;
}

int
corge ()
{
  return qux <string_view, int> ();
}

/* { dg-final { scan-assembler "foo" } } */
/* { dg-final { scan-assembler "hoo" } } */
/* { dg-final { scan-assembler "ioo" } } */
/* { dg-final { scan-assembler "koo" } } */
