/* PR optimization/13318 */
/* Origin: <bremner@unb.ca> */
/* Reduced testcase: Wolfgang Bangerth <bangerth@dealii.org> */

/* Verify that the big multiplier doesn't cause an integer
   overflow in the loop optimizer.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

struct S {
  int key;
  int rnext,rprev;
};
 
void foo(struct S* H)
{
  int i, k;
  for (i=0; i<2; i++){
    struct S* cell=H+k;
    cell->key=i*(0xffffffffUL/2);
    cell->rnext=k+(1-i);
    cell->rprev=k+(1-i);
  }
}
