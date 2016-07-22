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
  switch (a) { /* { dg-warning "statement will never be executed" } */
    int v[b];
  case 2: /* { dg-error "switch jumps into scope of identifier with variably modified type" } */
  default: /* { dg-error "switch jumps into scope of identifier with variably modified type" } */
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

/* Match extra informative notes.  */
/* { dg-message "note: switch starts here" "note: starts" { target *-*-* } 0 } */
/* { dg-message "note: '\[^\n'\]*' declared here" "note: declared" { target *-*-* } 0 } */
