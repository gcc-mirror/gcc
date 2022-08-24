/* { dg-do run { target int128 } } */
/* { dg-options "-O2 -fdump-tree-optimized -save-temps" } */

__attribute__ ((weak))
int f(char a)
{
   return  __builtin_memchr ("aEgiH", a, 5) == 0;
}

__attribute__ ((weak))
int g(char a)
{
  return a != 'a' && a != 'E' && a != 'g' && a != 'i' && a != 'H';
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
