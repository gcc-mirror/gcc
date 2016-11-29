/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-sources "abitest.S" } */

extern int clone(int (*fn)(void *), void *child_stack,
		 int flags, void *arg, ...);

int main (void)
{
  int a = clone ((void *) 1, (void *)2, 3, (void *) 4, 5, 6, 7);
  if (a != 28)
    return 1;
  return 0;
}
