extern int i;

/* While the OpenACC specification does allow for certain kinds of
   nesting, we don't support many of these yet.  */
void
f_acc_parallel (void)
{
#pragma acc parallel
  {
#pragma acc parallel /* { dg-bogus ".parallel. construct inside of .parallel. region" "not implemented" { xfail *-*-* } } */
    ;
#pragma acc kernels /* { dg-bogus ".kernels. construct inside of .parallel. region" "not implemented" { xfail *-*-* } } */
    ;
#pragma acc data /* { dg-error ".data. construct inside of .parallel. region" } */
    ;
#pragma acc update host(i) /* { dg-error ".update. construct inside of .parallel. region" } */
#pragma acc enter data copyin(i) /* { dg-error ".enter/exit data. construct inside of .parallel. region" } */
#pragma acc exit data delete(i) /* { dg-error ".enter/exit data. construct inside of .parallel. region" } */
  }
}

/* While the OpenACC specification does allow for certain kinds of
   nesting, we don't support many of these yet.  */
void
f_acc_kernels (void)
{
#pragma acc kernels
  {
#pragma acc parallel /* { dg-bogus ".parallel. construct inside of .kernels. region" "not implemented" { xfail *-*-* } } */
    ;
#pragma acc kernels /* { dg-bogus ".kernels. construct inside of .kernels. region" "not implemented" { xfail *-*-* } } */
    ;
#pragma acc data /* { dg-error ".data. construct inside of .kernels. region" } */
    ;
#pragma acc update host(i) /* { dg-error ".update. construct inside of .kernels. region" } */
#pragma acc enter data copyin(i) /* { dg-error ".enter/exit data. construct inside of .kernels. region" } */
#pragma acc exit data delete(i) /* { dg-error ".enter/exit data. construct inside of .kernels. region" } */
  }
}

#pragma acc routine
void
f_acc_routine (void)
{
#pragma acc parallel /* { dg-error "OpenACC region inside of OpenACC routine, nested parallelism not supported yet" } */
  ;
}
