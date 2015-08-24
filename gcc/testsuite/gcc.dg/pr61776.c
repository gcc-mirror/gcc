/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-generate" } */
/* { dg-require-profiling "-fprofile-generate" } */

#include <setjmp.h>

int cond1, cond2;

int goo() __attribute__((noinline));

int goo() {
 if (cond1)
   return 1;
 else
   return 2;
}

jmp_buf env;
int foo() {
 int a;

 setjmp(env);
 if (cond2)
   a = goo();
 else
   a = 3;
 return a;
}
