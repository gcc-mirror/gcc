/* Test diagnostics for VLA whose size folds to an integer constant at
   file scope; the diagnostic should be a pedwarn.  PR 39605.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

#define FIRST ((char*)0x80)
#define LAST  ((char*)0x86)

static int b[LAST-FIRST]; /* { dg-error "variably modified 'b' at file scope" } */
