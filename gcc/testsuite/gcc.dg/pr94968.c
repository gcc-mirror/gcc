/* PR c/94968 */
/* { dg-do compile } */

int
foo (void)
{ 
  __builtin_speculation_safe_value (1, x);	/* { dg-error "undeclared" } */
}						/* { dg-message "each undeclared identifier is reported only once" "" { target *-*-* } .-1 } */
