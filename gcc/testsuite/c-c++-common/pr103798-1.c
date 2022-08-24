/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-optimized -save-temps" } */

__attribute__ ((weak))
int
f (char a)
{
   return  __builtin_memchr ("a", a, 1) == 0;
}

__attribute__ ((weak))
int
g (char a)
{
  return a != 'a';
}

int
main ()
{
 for (int i = 0; i < 255; i++)
   if (f (i) != g (i))
     __builtin_abort ();

 return 0;
}

/* { dg-final { scan-assembler-not "memchr" } } */
