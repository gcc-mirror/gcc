/* Integer reductions.  */

#define n 1000

int
main(void)
{
  int v1, v2;

#pragma acc parallel reduction(+:v1,v2)
  ;
#pragma acc parallel reduction(+:v1,v2) copy(v1,v2)
  ;
#pragma acc parallel reduction(+:v1,v2) pcopy(v1,v2)
  ;
#pragma acc parallel reduction(+:v1,v2) present(v1,v2)
  ;
#pragma acc parallel reduction(+:v1,v2) copyin(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;
#pragma acc parallel reduction(+:v1,v2) pcopyin(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;
#pragma acc parallel reduction(+:v1,v2) copyout(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;
#pragma acc parallel reduction(+:v1,v2) pcopyout(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;
#pragma acc parallel reduction(+:v1,v2) create(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;
#pragma acc parallel reduction(+:v1,v2) pcreate(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;


#pragma acc serial reduction(+:v1,v2)
  ;
#pragma acc serial reduction(+:v1,v2) copy(v1,v2)
  ;
#pragma acc serial reduction(+:v1,v2) pcopy(v1,v2)
  ;
#pragma acc serial reduction(+:v1,v2) present(v1,v2)
  ;
#pragma acc serial reduction(+:v1,v2) copyin(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;
#pragma acc serial reduction(+:v1,v2) pcopyin(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;
#pragma acc serial reduction(+:v1,v2) copyout(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;
#pragma acc serial reduction(+:v1,v2) pcopyout(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;
#pragma acc serial reduction(+:v1,v2) create(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;
#pragma acc serial reduction(+:v1,v2) pcreate(v1,v2) /* { dg-warning "incompatible data clause with reduction" } */
  ;


  return 0;
}
