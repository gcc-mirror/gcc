/* PR middle-end/84723 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

__attribute__((target_clones ("avx", "default")))
void
foo (void)	/* { dg-error "clones for .target_clones. attribute cannot be created" } */
{		/* { dg-message "function .foo. can never be copied because it saves address of local label in a static variable" "" { target *-*-* } .-1 } */
  static void *p = &&lab;
  asm volatile ("" : "+m" (p) : : "memory");
lab:;
}
