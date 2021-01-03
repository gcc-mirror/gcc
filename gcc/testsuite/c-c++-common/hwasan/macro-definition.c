/* { dg-do compile } */
extern void testfunc(int);
int foo()
{
#ifndef __SANITIZE_HWADDRESS__
  testfunc(1);
#endif
  return 1;
}

/* { dg-final { scan-assembler-not "testfunc" } } */
