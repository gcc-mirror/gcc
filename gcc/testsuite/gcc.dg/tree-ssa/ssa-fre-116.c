/* PR127693 */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-fre3" } */

struct SV { long m_size; };

__attribute__((noipa))
long findString(long from, struct SV needle)
{
  __builtin_abort ();
}

__attribute__((noipa))
long findChar(long from)
{
  if (from == 0)
    return 1;
  return -1;
}

static long indexOf(struct SV s, long from)
{
  if (__builtin_constant_p(s.m_size) && s.m_size == 1)
    return findChar(from);
  return findString(from, s);
}

static int scan(struct SV sought)
{
  long n = sought.m_size;
  long idx = -n;
  int matched = 0;
  while ((idx = indexOf(sought, idx + n)) >= 0)
    ++matched;
  return matched;
}

int f(void) { return scan((struct SV){1}); }

int main()
{
  if (f() != 1)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "unreachable" "fre3" } } */
