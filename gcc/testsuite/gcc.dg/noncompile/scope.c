static int v = 3;

f ()
{
  int v = 4;
  {
    extern int v; /* { dg-error "static" } */  
    if (v != 3)
      abort ();
  }
}

main ()
{
  f ();
  exit (0);
}
