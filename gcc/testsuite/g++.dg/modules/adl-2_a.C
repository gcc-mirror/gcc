// { dg-module-do run }
// { dg-additional-options -fmodules-ts }
export module foo;
// { dg-module-cmi foo }

export template <typename T>
int TPL (T const &t)
{
  return frob (t);
}
