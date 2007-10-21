/* { dg-do assemble { target { ! { powerpc-ibm-aix* } } } } */
/* { dg-options "-fleading-underscore -funwind-tables" } */

void func(void) __asm("_func");
void _func(int x) {}
void func(void) {}

