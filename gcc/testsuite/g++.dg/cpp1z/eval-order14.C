// PR c++/122772
// { dg-do run }

struct S { int y : 7; } s;
struct T { int x; int z : 7; } t;
int cnt, mode;

int
foo ()
{
#if __cplusplus >= 201703L
  if (cnt != 1 + (mode & 1))
    __builtin_abort ();
#endif
  ++cnt;
  return 2;
}

int
bar ()
{
#if __cplusplus >= 201703L
  if (cnt != 0)
    __builtin_abort ();
#endif
  ++cnt;
  return 40;
}

S &
baz ()
{
#if __cplusplus >= 201703L
  if (cnt != 2 + (mode & 1))
    __builtin_abort ();
#endif
  if (mode & 2)
    __builtin_abort ();
  ++cnt;
  return s;
}

T &
qux ()
{
#if __cplusplus >= 201703L
  if (cnt != 2 + (mode & 1))
    __builtin_abort ();
#endif
  if ((mode & 2) == 0)
    __builtin_abort ();
  ++cnt;
  return t;
}

bool
fred (bool x)
{
#if __cplusplus >= 201703L
  if (cnt != 1)
    __builtin_abort ();
#endif
  ++cnt;
  return x;
}

void
plugh (bool b)
{
  S x;
  x.y = 5;
  mode = 0;
  cnt = 0;
  (x.y = foo ()) += bar ();
  if (cnt != 2 || x.y != 42)
    __builtin_abort ();
  x.y = 5;
  cnt = 0;
  (baz ().y = foo ()) += bar ();
  if (cnt != 3 || s.y != 42)
    __builtin_abort ();
  s.y = 5;
  mode = (b ? 0 : 2);
  cnt = 0;
  (b ? (baz ().y = foo ()) : (qux ().z = foo ())) += bar ();
  if (cnt != 3 || (b ? (s.y != 42) : (t.z != 42)))
    __builtin_abort ();
  s.y = 5;
  t.z = 5;
  mode |= 1;
  cnt = 0;
  (fred (b) ? (baz ().y = foo ()) : (qux ().z = foo ())) += bar ();
  if (cnt != 4 || (b ? (s.y != 42) : (t.z != 42)))
    __builtin_abort ();
  s.y = 5;
  t.z = 5;
  mode = 0;
  cnt = 0;
  ++x.y += bar ();
  if (cnt != 1 || x.y != 46)
    __builtin_abort ();
  cnt = 0;
  x.y = 9;
  --x.y += bar ();
  if (cnt != 1 || x.y != 48)
    __builtin_abort ();
}

int
main ()
{
  plugh (false);
  plugh (true);
}
