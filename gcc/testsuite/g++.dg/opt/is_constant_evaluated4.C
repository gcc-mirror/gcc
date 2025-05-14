// PR c++/114970
// { dg-do compile { target c++17 } }
// { dg-additional-options "-O -Wunused-value" }

struct sv
{
  const char* str;
  unsigned len;

  constexpr sv(const char *p): str(p), len(0)
  {
    if (__builtin_is_constant_evaluated ()) { len = 42; }
  }
};

int main()
{
  sv s ("foo");
  return s.len;
}
