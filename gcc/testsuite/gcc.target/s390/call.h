/* Common code for testing the function call code generation.  */

__attribute__ ((noipa)) void
foo (void)
{
  return;
}

void *
usefoo (void)
{
  foo ();
  return foo;
}

__attribute__ ((noipa)) static void
foostatic (void)
{
  return;
}

void *
usefoostatic (void)
{
  foostatic ();
  return foostatic;
}

__attribute__ ((weak)) void fooweak (void);

void *
usefooweak (void)
{
  fooweak ();
  return fooweak;
}

__attribute__ ((__used__, section (".foos"), aligned (sizeof (void *))))
static void
*foos[] = { foo, foostatic, fooweak };
