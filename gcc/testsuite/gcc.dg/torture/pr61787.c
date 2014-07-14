/* { dg-do run } */

unsigned int a = 1;

int
fn1 ()
{
  char b;
  for (;;)
    {
      for (b = 0; b < 5; b++)
	if (!a - (unsigned int) b)
	  return 0;
      a = 0;
    }
}

int
main ()
{
  fn1 ();

  if (a != 1) 
    __builtin_abort (); 

  return 0;
}
