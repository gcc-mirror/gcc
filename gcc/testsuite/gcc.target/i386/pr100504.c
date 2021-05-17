/* PR middle-end/100504 */
/* { dg-do compile } */

__attribute__((target_clones(0)))
foo()
{ /* { dg-error ".target_clones. attribute argument not a string constant" } */
}
