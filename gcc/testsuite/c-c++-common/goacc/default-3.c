/* OpenACC default (none) clause.  */

void f1 ()
{
  int f1_a = 2;
  float f1_b[2];
  
#pragma acc kernels default (none) /* { dg-message "enclosing OpenACC .kernels. construct" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" } */
  }
#pragma acc parallel default (none) /* { dg-message "enclosing OpenACC .parallel. construct" } */
  {
    f1_b[0] /* { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" } */
      = f1_a; /* { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" } */
  }
}
