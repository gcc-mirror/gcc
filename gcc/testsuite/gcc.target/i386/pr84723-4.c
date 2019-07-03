/* PR middle-end/84723 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

__attribute__((target_clones ("avx", "default")))
__attribute__((naked))
void
foo (void)	/* { dg-error "clones for .target_clones. attribute cannot be created" } */
{		/* { dg-message "function .foo. can never be copied because it has .noclone. attribute" "" { target *-*-* } .-1 } */
}
