/* PR libstdc++/88101 */

#include <new>

struct S { char a; short b; char c; long long d; char e; decltype (nullptr) f; char g; };
alignas (S) unsigned char buf1[sizeof (S)];
alignas (S) unsigned char buf2[sizeof (S)];

template <int N>
void
foo ()
{
  __builtin_clear_padding ((S *) buf2);
}

void
bar (S *s)
{
  s->a = -1; s->b = -1; s->c = -1; s->d = -1; s->e = -1; s->g = -1;
}

int
main ()
{
  S *s1 = new (buf1) S;
  S *s2 = new (buf2) S;
  __builtin_memset (s1, 0, sizeof (S));
  __builtin_memset (s2, ~0, sizeof (S));
  bar (s1);
  bar (s2);
  foo <0> ();
  if (__builtin_memcmp (s1, s2, sizeof (S)) != 0)
    __builtin_abort ();
}
