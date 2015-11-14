// PR bootstrap/68346
// { dg-options -Wtautological-compare }

bool f(unsigned u)
{
  return ((unsigned)(int)u == u);
}
