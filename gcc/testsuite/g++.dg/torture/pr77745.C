// { dg-do run }

#ifndef NOINLINE
#define NOINLINE /* */
#endif

inline void* operator new(__SIZE_TYPE__, void* __p) noexcept { return __p; }

long NOINLINE foo(char *c1, char *c2)
{
  long *p1 = new (c1) long;
  *p1 = 100;
  long long *p2 = new (c2) long long;
  *p2 = 200;
  long *p3 = new (c2) long;
  *p3 = 200;
  return *p1;
}
int main()
{
  union {
      char c;
      long l;
      long long ll;
  } c;
  if (foo(&c.c, &c.c) != 200)
    __builtin_abort();
}
