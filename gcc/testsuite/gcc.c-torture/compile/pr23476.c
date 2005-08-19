int h(int);
int t;
static inline int f(const int i)
{
  int tt = i;
  _Bool a = i < t;
  if (a)
    return h(t);
  return 9;
}
int g(void)
{
  return f(0x7FFFFFFF);
}
