inline int
f (int x)
{
  return (x + 1);
}
 
int
main (void)
{
  int a = 0 ;
 
  while ( (f(f(f(f(f(f(f(f(f(f(1))))))))))) + a < 12 )
    {
      a++;
      exit (0);
    }
  if (a != 1)
    abort();
}
