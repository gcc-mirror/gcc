// { dg-do run }

typedef __SIZE_TYPE__ size_t;
inline void *operator new (size_t, void *p) noexcept { return p; }

struct V
{
  long *s = nullptr, *f = nullptr;
  V () = default;
  V (long *b, long *e) : s (b), f (e) {}
  V (const V &o)
  {
    size_t n = o.f - o.s;
    s = n ? (long *) __builtin_operator_new (n * sizeof (long)) : nullptr;
    if (n)
      __builtin_memcpy (s, o.s, n * sizeof (long));
    f = s + n;
  }
  V (V &&o) : s (o.s), f (o.f) {}
};

struct VV
{
  V *s = nullptr, *f = nullptr;
  void push_back (const V &x)
  {
    size_t n = f - s;
    V *ns = (V *) __builtin_operator_new ((n + 1) * sizeof (V));
    new (ns + n) V (x);
    for (V *p = s, *d = ns; p != f; ++p, ++d)
      new (d) V (static_cast<V &&> (*p));
    s = ns;
    f = ns + n + 1;
  }
};

__attribute__((noipa)) long *
f (const V &a, const V &b)
{
  VV v;
  v.push_back (a);
  v.push_back (b);
  v.push_back (a);
  return v.s[1].s;
}

int
main ()
{
  long x = 2;
  V a, b (&x, &x + 1);
  if (f (a, b) == nullptr)
    __builtin_abort ();
}
