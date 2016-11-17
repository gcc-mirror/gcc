/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-sources "abitest.S" } */

extern long tsyscall (long int sysnum, ...);

int main (void)
{
  long a;

  a = tsyscall (1, 2, 3, 4, 5, 6, 7);

  if (a != 28)
    return 1;
  return 0;
}
