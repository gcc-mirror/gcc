/* { dg-options "-O2 -fdump-ipa-modref"  } */
/* { dg-do link } */
int *ptr;
void linker_error ();
int
main ()
{
  int a;
  __attribute__((noinline)) int test2 ()
  {
    ptr = 0;
    return a;
  }
  a = 1;
  test2 ();
  if (a != 1)
    linker_error ();
  return 0;
}
/* { dg-final { scan-ipa-dump "Static chain flags: no_direct_clobber no_indirect_clobber no_direct_escape no_indirect_escape not_returned_directly no_indirect_read" "modref" } } */
