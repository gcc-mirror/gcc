/* { dg-do compile } */
/* { dg-options "-mgeneral-regs-only" } */
/* PR target/113486 */


/* __builtin_apply should not use FP registers if 
   general registers only mode is requested. */
void
foo (void)
{
  __builtin_apply (foo, 0, 0);
}
