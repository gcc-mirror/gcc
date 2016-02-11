/* { dg-do run } */

int a;
extern void abort (void);
int main ()
{
  int c = 0;
  for (; a < 13; ++a)
    c = (signed char)c - 11;
  if (c != 113)
    abort ();
  return 0;
}
