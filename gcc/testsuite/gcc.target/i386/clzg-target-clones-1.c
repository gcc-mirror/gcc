/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O2 -mlzcnt" } */

/* With -mlzcnt __builtin_clzg (x, 32) becomes .CLZ (x, 32).  The
   arch=x86-64 clone has no LZCNT and must still return 32 for 0.  */

__attribute__((target_clones ("arch=x86-64", "default"))) int
f (unsigned int x)
{
  return __builtin_clzg (x, 32);
}

int
main (void)
{
  if (f (0) != 32 || f (1) != 31)
    __builtin_abort ();
  return 0;
}
