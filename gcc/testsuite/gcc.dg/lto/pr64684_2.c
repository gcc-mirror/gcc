/* { dg-options "-O0" } */

extern void fn1 (void);

int a;

int
main ()
{
  fn1 ();

  if (a != 0) 
    __builtin_abort (); 

  return 0;
}
