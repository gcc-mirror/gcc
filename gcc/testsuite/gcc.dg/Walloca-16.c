/* PR tree-optimization/84224 */
/* { dg-do compile } */
/* { dg-prune-output "conflicting types for built-in" } */
/* { dg-options "-O0 -Walloca" } */

void *alloca ();
__typeof__(alloca ()) a () { return alloca (); }

/* { dg-prune-output "\\\[-Wbuiltin-declaration-mismatch]" } */
