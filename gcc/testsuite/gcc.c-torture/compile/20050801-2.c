/* { dg-skip-if "requires alloca" { ! alloca } { "-O0" } { "" } } */
__inline int f(int i)
{
  struct {
    int t[i];
  } t;
  return sizeof(t.t[i--]);
}

int g(int i)
{
  return f(i);
}
