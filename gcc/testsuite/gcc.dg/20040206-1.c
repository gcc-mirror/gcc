/* { dg-do compile } */
/* { dg-options "-O2 -W -Wall" } */
/*  PR c/13127
    On the tree-ssa this used to warn about an anonymous
    uninitialized variable.

    The warning about "no return statement in function
    returning non-void" is PR 13000. */

static int foo (int a __attribute__((unused)) ) { } /* { dg-warning "control reaches end of non-void" } */
int main (void) { return foo (0); }
