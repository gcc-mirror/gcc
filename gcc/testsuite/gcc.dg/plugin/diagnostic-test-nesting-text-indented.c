/* { dg-do compile } */
/* { dg-options "-fdiagnostics-set-output=text:experimental-nesting=yes" } */

extern void foo (void);

void test_nesting (void)
{
  foo (); /* { dg-error "top-level error" } */
}

/* { dg-begin-multiline-output "" }
  * note: child 0
    * note: grandchild 0 0
    * note: grandchild 0 1
    * note: grandchild 0 2
  * note: child 1
    * note: grandchild 1 0
    * note: grandchild 1 1
    * note: grandchild 1 2
  * note: child 2
    * note: grandchild 2 0
    * note: grandchild 2 1
    * note: grandchild 2 2
   { dg-end-multiline-output "" } */
