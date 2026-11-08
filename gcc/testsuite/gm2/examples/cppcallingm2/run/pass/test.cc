#include <stdio.h>
#include <m2/m2iso/m2rts.h>

#define USER_LIB NULL

/* Add the runtime dependency for this file on modules a, b and c.  */

void
dep (void)
{
  m2iso_M2RTS_RequestDependant (__FILE__, USER_LIB, "c", USER_LIB);
  m2iso_M2RTS_RequestDependant (__FILE__, USER_LIB, "b", USER_LIB);
  m2iso_M2RTS_RequestDependant (__FILE__, USER_LIB, "a", USER_LIB);
}

void
init (int, char *[], char *[])
{
  printf ("test.c:init\n");
}

void
fini (int, char *[], char *[])
{
  printf ("test.c:fini\n");
}

void
construct_scaffold (int argc, char *argv[], char *envp[])
{
  m2iso_M2RTS_RegisterModule (__FILE__, USER_LIB,
                              init, fini, dep);
  m2iso_M2RTS_ConstructModules (__FILE__, USER_LIB,
                                DEFAULT_RUNTIME_MODULE_OVERRIDE,
                                argc, argv, envp);
}

void
deconstruct_scaffold (int argc, char *argv[], char *envp[])
{
  m2iso_M2RTS_DeconstructModules (__FILE__, USER_LIB,
                                  argc, argv, envp);
}

int
main (int argc, char *argv[], char *envp[])
{
  printf ("main starts\n");
  construct_scaffold (argc, argv, envp);
  printf ("main application goes here\n");
  deconstruct_scaffold (argc, argv, envp);
  printf ("main tidying up\n");
  return 0;
}
