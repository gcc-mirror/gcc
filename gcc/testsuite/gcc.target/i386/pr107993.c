/* PR c/107993 */
/* { dg-do compile } */

typedef union { int x; } u;
__attribute__((target_clones("arch=alderlake",!"default")))
int f (u *x)
{ /* { dg-error ".target_clones. attribute argument not a string constant" } */
  return 0;
}
