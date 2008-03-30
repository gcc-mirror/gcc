/* PR c/35748 */

union U { void x[1]; }; /* { dg-error "array of voids" } */

void foo()
{
  (union U)0; /* { dg-error "type not present" } */
}
