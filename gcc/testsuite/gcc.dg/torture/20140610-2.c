/* { dg-do compile } */

extern void abort (void);

int a;
int *p = &a;

void test (void)
{
  if (a != 1)
    abort ();
}
