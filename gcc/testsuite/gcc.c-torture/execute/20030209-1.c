/* { dg-require-stack-size "8*100*100" } */

void abort (void);
void exit (int);

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
