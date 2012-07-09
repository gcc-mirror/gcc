/* Test for tree-checking error when initializing a variable-length array
   (not allowed): constructor_max_index needs to be an INTEGER_CST.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk>.  */
/* { dg-do compile } */
/* { dg-options "" } */

int a;

void
foo (void)
{
  int x[a] = { 1 }; /* { dg-error "variable-sized object may not be initialized" "VLA init" } */
  /* { dg-warning "excess elements in array initializer" "excess" { target *-*-* } 12 } */
  /* { dg-warning "near initialization" "near" { target *-*-* } 12 } */
}
