/* Integer reductions.  */

#define n 1000

int
main(void)
{
  int v1;

#pragma acc parallel reduction(+:v1) private(v1) /* { dg-error "appears more than once in data clauses" } */
  ;
#pragma acc parallel reduction(+:v1) firstprivate(v1) /* { dg-error "appears more than once in data clauses" } */
  ;

  return 0;
}
