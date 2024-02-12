// PR c++/113706
/* A variant of the pr103798-2.c test with const void * memchr return type, as
   specified by the C++ standard.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-optimized -save-temps" } */

extern "C" const void *memchr (const void *, int, __SIZE_TYPE__); // { dg-bogus "built-in" }

__attribute__ ((weak))
int
f (int a)
{
   return memchr ("aE", a, 2) != 0;
}

__attribute__ ((weak))
int
g (char a)
{
  return a == 'a' || a == 'E';
}

int
main ()
{
 for (int i = 0; i < 255; i++)
   if (f (i + 256) != g (i + 256))
     __builtin_abort ();

 return 0;
}

/* { dg-final { scan-assembler-not "memchr" } } */
