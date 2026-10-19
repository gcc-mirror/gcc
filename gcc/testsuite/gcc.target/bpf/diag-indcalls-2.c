/* Verify indirect calls are diagnosed at the call itself, and once per
   call, also when optimizing.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-callx" } */

int (*fnp) (int, int);

int
foo (void (*arg) (void))
{
  arg (); /* { dg-error "indirect call in function" } */
  return (*fnp) (1, 2); /* { dg-error "indirect call in function" } */
}
