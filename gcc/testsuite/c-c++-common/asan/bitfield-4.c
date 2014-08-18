/* Check that Asan correctly instruments bitfields with round offset.  */

/* { dg-do run } */
/* { dg-shouldfail "asan" } */

struct A
{
  char base;
  int : 0;
  int x : 8;
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
