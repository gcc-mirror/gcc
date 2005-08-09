/* We used to ICE in gimple-lower because we
   would produce __t (VAR_DECL) as a statement in the
   instruction stream which is not valid. */

static inline int f(int i)
{
  const int __t = (__t);
}
int g(void)
{
  return f(0);
}
