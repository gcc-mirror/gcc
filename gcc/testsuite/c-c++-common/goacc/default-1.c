/* OpenACC default clause: valid syntax.  */

void f1 ()
{
#pragma acc kernels default (none)
  ;
#pragma acc parallel default (none)
  ;
}
