foo (a, p)
     register int a;
     int *p;
{

  for (a = 10000000; a >= *p; a--)
    ;
}

main ()
{
  int a;
  foo (a, a);
}
