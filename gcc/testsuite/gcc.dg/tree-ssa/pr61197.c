/* { dg-do run } */
/* { dg-options "-O3" } */

int a, b = 1, *c = &a; 

int
foo ()
{
  if (b)
    b |= 1;
  else
    {
      b = 1;
      return 0;
    }
  return 1;
}

int
main ()
{
  *c = foo ();
  if (a != 1) 
    __builtin_abort (); 
  return 0;
}
