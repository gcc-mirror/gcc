/* Test for labels and VM declarations: bug 12913.
   switch statements must not jump into the scope of VM declarations.

   c99-vla-jump-1.c tests with just that label and goto, VLAs.
   c99-vla-jump-2.c tests with many other labels and gotos, VLAs.
   c99-vla-jump-3.c tests with just that label and goto, VM.
   c99-vla-jump-4.c tests with many other labels and gotos, VM.
   c99-vla-jump-5.c tests with switch statements.  */

/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
f (int a, int b)
{
  switch (a) {
    int v[b];
  case 2: /* { dg-error "error: case label in scope of identifier with variably modified type not containing enclosing switch statement" } */
  default: /* { dg-error "error: 'default' label in scope of identifier with variably modified type not containing enclosing switch statement" } */
  switch (a)
    {
    case 4:
      { int z[b]; }
    default:
      ;
      int w[b];
    }
  }
}
