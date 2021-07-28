/* Common code for testing the TLS code generation.  */

__thread int
foo;

int
setfoo (int x)
{
  int result = foo;
  foo = x;
  return result;
}

static __thread int
foostatic;

int
setfoostatic (int x)
{
  int result = foostatic;
  foostatic = x;
  return result;
}
