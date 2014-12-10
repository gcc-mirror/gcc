/* Check BIT_FIELD_REF.  */

/* { dg-do run } */
/* { dg-shouldfail "asan" } */

struct A
{
  int y : 20;
  int x : 13;
};

int __attribute__ ((noinline, noclone))
f (void *p) {
  return ((struct A *)p)->x != 0;
}

int
main ()
{
  int a = 0;
  return f (&a);
}

/* { dg-output "ERROR: AddressSanitizer: stack-buffer-overflow" } */
