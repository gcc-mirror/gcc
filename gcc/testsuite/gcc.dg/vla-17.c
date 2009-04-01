/* Test diagnostics for VLA whose size folds to an integer constant at
   file scope.  PR 39605.  */
/* { dg-do compile } */
/* { dg-options "" } */

#define FIRST ((void*)0x80)
#define LAST  ((void*)0x86)

static int b[LAST-FIRST]; /* { dg-warning "variably modified 'b' at file scope" } */
