// { dg-additional-options -fmodules-ts }
export module inter;
// { dg-module-cmi inter }

export template <typename T>
int TPL (T &t)
{
  return fn (t);
}
