/* { dg-lto-do run } */
/* { dg-lto-options { {-O2 -flto-partition=max -fdump-ipa-modref -fno-ipa-sra -flto} } } */
extern void copy (int *a, int *b);
extern void barrier ();
extern int *ptr;
int
main()
{
  int a = 1, b = 2;
  copy (&a,&b);
  barrier ();
  *ptr = 1;
  if (!__builtin_constant_p (b == 2))
    __builtin_abort ();
  return 0;
}
/* { dg-final { scan-wpa-ipa-dump "parm 1 flags: no_direct_clobber no_direct_escape"  "modref"  } } */
