/* This test does not compile on mips-irix6 using the native assembler,
   though it does work with gas.  See PR6200.  Since we cannot (???)
   distinguish which assembler is being used, always pass -S for
   irix.  */
/* { dg-options "-w -S" { target mips*-*-irix* } } */

/* Verify that we can narrow the storage associated with label diffs.  */

int foo (int a)
{
  static const short ar[] = { &&l1 - &&l1, &&l2 - &&l1 };
  void *p = &&l1 + ar[a];
  goto *p;
 l1:
  return 1;
 l2:
  return 2;
}
