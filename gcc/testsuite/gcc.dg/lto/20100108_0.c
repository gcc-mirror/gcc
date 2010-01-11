/* { dg-lto-do assemble } */
/* The problem with this testcase is that we were missing an undefined
   reference to flag_exceptions. This can be tested with
   GNUTARGET=plugin nm --plugin liblto_plugin.so 20100108_0.o
   but we don't have support in the testsuite for doing it. */

extern int flag_exceptions;
static int *foo = &flag_exceptions;
int **bar = &foo;
