/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__((target_clones("default", "random-arch-string")))
int foo () /* { dg-warning "invalid version .*random-arch-string.*target_clones" } */
{
	return 1;
}
