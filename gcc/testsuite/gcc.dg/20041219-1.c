/* PR18191 Struct member is not getting default-initialized.
   Origin: Grigory Zagorodnev  <grigory.zagorodnev@intel.com>  */

/* { dg-do run } */

extern int printf (__const char *__restrict __format, ...);

typedef struct S {
  const char* s;
  int         i;
} S;

void
foo (void)
{
  S dummy[2];
  unsigned i;

  /* Put some garbage on the stack.  */
  for (i = 0; i < sizeof(dummy); i++)
    ((char *)&dummy)[i] = -1;
}

int
bar (void)
{
  /* Allocate object on the stack.  */
  S obj[2] = { {"m0"}, {"m1"} };

  /* Assume fields those not explicitly initialized
     are default initialized to 0 [8.5.1/7 and 8.5/5].  */
  if (obj[0].i == 0)
    return 0;
  else
    {
      printf("Failed: obj[0].i == '%d', expecting '0'\n", obj[0].i);
      return 1;
    }
}

int
main (void)
{
  foo();
  return bar();
}

