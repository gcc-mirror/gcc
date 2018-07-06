/* PR tree-optimization/84224 */
/* { dg-do compile } */
/* { dg-options "-O0 -Walloca" } */

void *alloca ();
__typeof__(alloca ()) a () { return alloca (); }
