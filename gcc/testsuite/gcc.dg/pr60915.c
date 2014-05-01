/* PR c/60915 */
/* { dg-do compile } */

void /* { dg-error "attributes should be specified before the declarator in a function definition" } */
foo (void) __attribute__((__visibility__("default")))
{
}
