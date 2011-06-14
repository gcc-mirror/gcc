typedef unsigned T;
struct S
{
  T foo (void);
  static unsigned s1[16];
};
T
S::foo ()
{
  T u = *(T *) (s1 + 10);
  return u;
}
