/* Test for tree-checking error when initializing a variable-length array
   (not allowed): constructor_max_index needs to be an INTEGER_CST.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk>.  */
/* { dg-do compile } */
/* { dg-options "" } */

int a;

void
foo (void)
{
  int x[a] = { 1 }; /* { dg-error "init" "VLA init" } */
}
