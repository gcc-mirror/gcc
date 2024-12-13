/* { dg-do compile } */
/* { dg-options "-fdiagnostics-set-output=text:experimental-nesting=yes" } */

extern void foo (void);

void test_nesting (void)
{
  foo (); /* { dg-error "top-level error" } */
}

/* { dg-begin-multiline-output "" }
  * child 0
    * grandchild 0 0
    * grandchild 0 1
    * grandchild 0 2
  * child 1
    * grandchild 1 0
    * grandchild 1 1
    * grandchild 1 2
  * child 2
    * grandchild 2 0
    * grandchild 2 1
    * grandchild 2 2
   { dg-end-multiline-output "" } */
