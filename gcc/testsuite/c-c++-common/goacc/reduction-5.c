/* Integer reductions.  */

#define n 1000

int
main(void)
{
  int v1;

#pragma acc parallel reduction(+:v1) private(v1) /* { dg-error "invalid private reduction" } */
  ;
#pragma acc parallel reduction(+:v1) firstprivate(v1) /* { dg-error "invalid private reduction" } */
  ;

  return 0;
}
