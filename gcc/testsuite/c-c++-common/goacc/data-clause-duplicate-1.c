void
fun (void)
{
  float *fp;
#pragma acc parallel copy(fp[0:2],fp[0:2]) /* { dg-error "'fp' appears more than once in data clauses" } */
  ;
#pragma acc kernels present_or_copyin(fp[3]) present_or_copyout(fp[7:4]) /* { dg-error "'fp' appears more than once in data clauses" } */
  ;
#pragma acc data create(fp[:10]) deviceptr(fp) /* { dg-error "'fp' appears more than once in data clauses" } */
  ;
#pragma acc data create(fp) present(fp) /* { dg-error "'fp' appears more than once in data clauses" } */
  ;
}
