/* { dg-do run { target int128 } } */
/* { dg-options "-O2 -fdump-tree-optimized -save-temps" } */

__attribute__ ((weak))
int f(int a)
{
   return  __builtin_memchr ("aEgiHx19ABC", a, 8) != 0;
}

__attribute__ ((weak))
int g(char a)
{
  return (a == 'a' || a == 'E' || a == 'g' || a == 'i' || a == 'H'
	  || a == 'x' || a == '1' || a == '9');
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
