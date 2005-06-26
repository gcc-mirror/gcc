#if __SCHAR_MAX__ == 127 && __INT_MAX__ >= 2147483647
struct S { char buf[72*1024*1024]; };
#else
struct S { char buf[64]; };
#endif

extern void bar (struct S);

struct S s;

int
foo (void)
{
  bar (s);
  return 0;
}
