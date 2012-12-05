/* PR c/55570 */
/* { dg-do compile } */

char array[16] __attribute__((aligned (SOME_NOT_DEFINED_MACRO))); /* { dg-error "requested alignment is not an integer constant" } */
