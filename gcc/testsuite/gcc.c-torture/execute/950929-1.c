void abort (void);
void exit (int);

int f (char *p) { }

int
main (void)
{
  char c;
  char c2;
  int i = 0;
  char *pc = &c;
  char *pc2 = &c2;
  int *pi = &i;

  *pc2 = 1;
  *pi = 1;
  *pc2 &= *pi;
  f (pc2);
  *pc2 = 1;
  *pc2 &= *pi;
  if (*pc2 != 1)
    abort ();
  exit (0);
}
