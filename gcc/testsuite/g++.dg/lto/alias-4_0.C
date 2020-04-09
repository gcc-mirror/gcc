/* { dg-lto-do run } */
/* { dg-lto-options { { -O3 -flto -fno-early-inlining -fdump-ipa-cgraph} } } */
__attribute__ ((used))
short *ptr_init, **ptr=&ptr_init;

__attribute__ ((used))
struct a {
  int *aptr;
  /* On ARM va_list is an anonymous structure containing pointer. 
     This disable ODR TBAA on it.  */
  short b;
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
/* On ARM the testcase used to fail because struct a got in conflict with builtin
   va_list type.  Check that this does not happen.  */
/* { dg-final { scan-wpa-ipa-dump-not "ODR and non-ODR type conflict" "cgraph"  } } */
