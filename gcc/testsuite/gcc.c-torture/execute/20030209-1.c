#ifdef STACK_SIZE
#if STACK_SIZE < 8*100*100
#define SKIP
#endif
#endif

#ifndef SKIP
double x[100][100];
int main ()
{
  int i;

  i = 99;
  x[i][0] = 42;
  if (x[99][0] != 42)
    abort ();
  exit (0);
}
#else
int
main ()
{
  exit (0);
}
#endif
