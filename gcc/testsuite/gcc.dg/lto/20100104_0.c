/* { dg-lto-do assemble } */
/* The problem with this testcase is that we were missing an undefined
   reference to c_common_attribute_table. This can be tested with
   GNUTARGET=plugin nm --plugin liblto_plugin.so 20100104_0.o
   but we don't have support in the testsuite for doing it. */

extern int c_common_attribute_table[];
void *foobar = c_common_attribute_table;
