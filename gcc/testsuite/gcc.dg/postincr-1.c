/* Simple test for proper postincrement semantics.  */
/* { dg-do run } */

extern void abort (void);

int i;
int c;
int *f ()
{
  ++c;
  return &i;
}

int main ()
{
  int r;
  r = (*f())++;
  if (!(r == 0 && i == 1 && c == 1))
    abort ();
  return 0;
}
