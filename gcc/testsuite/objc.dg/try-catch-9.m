/* Check that taking the address of a local variable marked 'volatile'
   by the compiler does not generate untoward errors.  */
/* Developed by Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

void foo (int *arg1, int *arg2)
{
  *arg1 = *arg2;
}

void bar (int arg) {
  int rcvr;

  @try {
    rcvr = arg;
  }
  @finally {
    int *rcvr0 = &rcvr;
    foo (rcvr0, &arg);
  }
}

