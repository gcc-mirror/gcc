/* PR middle-end/18921 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

int foo (int k)
{
  unsigned char j = 0;
  (k ? 0 : j++) == -1;
  return j;
}

int main ()
{
  if (!foo (0))
    abort ();
  return 0;
}

