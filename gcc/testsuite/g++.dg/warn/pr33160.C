// PR 33160
// { dg-do compile }
// { dg-options "-Wall -Wextra -Wpointer-arith -pedantic -Wconversion" }

typedef int __attribute__((mode(pointer))) intptr_t;
int foo(void)
{
 intptr_t t = 0;
 if (t != ((intptr_t)__null)) t = 1;
 return 0;
}

