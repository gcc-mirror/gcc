/* PR c/96596 - ICE on a declaration of a built-in with invalid array
   argument
   { dg-do compile } */

void __builtin_abort (int[foo]);            /* { dg-error "'foo' undeclared" } */
void __builtin_trap (int[__SIZE_MAX__]);    /* { dg-error "size of unnamed array is too large" } */
void __builtin_unreachable (int[][]);       /* { dg-error "array type has incomplete element type" } */

void __builtin_exit (int, int[+]);          /* { dg-error "expected expression before" } */

/* { dg-prune-output "\\\[-Wbuiltin-declaration-mismatch" }  */

