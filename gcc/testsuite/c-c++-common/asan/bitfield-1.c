/* Check that Asan correctly instruments bitfields with non-round size.  */

/* { dg-do run } */
/* { dg-shouldfail "asan" } */

struct A
{
  char base;
  int : 4;
  long x : 7;
};

int __attribute__ ((noinline, noclone))
f (void *p) {
  return ((struct A *)p)->x;
}

int
main ()
{
  char a = 0;
  return f (&a);
}

/* { dg-output "ERROR: AddressSanitizer: stack-buffer-overflow" } */
