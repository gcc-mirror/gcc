/* { dg-do run } */
/* { dg-options "-O1" } */ 

void abort (void);

int main ()
{
  int a;
  volatile int *b = &a;
  a = 1;
  if (*b != 1)
    abort ();
  return 0;
}
