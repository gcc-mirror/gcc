/* { dg-lto-do run } */
/* { dg-lto-options { { -O3 -flto -fno-early-inlining } } } */
__attribute__ ((used))
short *ptr_init, **ptr=&ptr_init;

__attribute__ ((used))
struct a {
  int *aptr;
} a, *aptr=&a;

void
write_ptr ()
{
  *aptr = a;
}

__attribute__ ((used))
void
test ()
{
  *ptr = (short int *)0;
  write_ptr ();
  if (!__builtin_constant_p (*ptr == (void *)0))
    __builtin_abort ();
}
int
main()
{
  test ();
  return 0;
}
