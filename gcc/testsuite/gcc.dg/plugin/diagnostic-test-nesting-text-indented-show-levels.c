/* { dg-do compile } */
/* { dg-options "-fdiagnostics-set-output=text:experimental-nesting=yes,experimental-nesting-show-levels=yes" } */

extern void foo (void);

void test_nesting (void)
{
  foo (); /* { dg-error "top-level error" } */
}

/* { dg-begin-multiline-output "" }
  * (level 1):child 0
    * (level 2):grandchild 0 0
    * (level 2):grandchild 0 1
    * (level 2):grandchild 0 2
  * (level 1):child 1
    * (level 2):grandchild 1 0
    * (level 2):grandchild 1 1
    * (level 2):grandchild 1 2
  * (level 1):child 2
    * (level 2):grandchild 2 0
    * (level 2):grandchild 2 1
    * (level 2):grandchild 2 2
   { dg-end-multiline-output "" } */
