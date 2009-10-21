/* { dg-do run } */
/* { dg-options "-Os" } */

extern void abort (void);

unsigned int a;
int b, c;

void
foo (void)
{
  b = 0;
  do {
    for (a = -13; a == 0; a = (unsigned short)a)
      c = 1;
    b++;
  } while (b == 0);
}

int
main ()
{
  foo ();
  if (a != -13)
    abort ();
  return 0;
}
