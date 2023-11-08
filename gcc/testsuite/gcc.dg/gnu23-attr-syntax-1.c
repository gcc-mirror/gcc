/* Test C23 attribute syntax.  Basic tests of valid uses of empty
   attributes with GNU C features.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

/* Attributes can be used in declarations after __extension__, and
   before asm statements.  */

__extension__ [[]] int a;

void
f (void)
{
  __extension__ [[]] int b;
  [[]] asm ("");
}
