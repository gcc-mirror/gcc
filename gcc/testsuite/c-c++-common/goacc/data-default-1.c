/* { dg-do compile } */


int main ()
{
  int n = 2;
  int ary[2];
  
#pragma acc parallel default (none) /* { dg-message "'parallel' construct" 2 } */
  {
    ary[0] /* { dg-error "not specified in enclosing" } */
      = n; /* { dg-error "not specified in enclosing" } */
  }

#pragma acc kernels default (none) /* { dg-message "'kernels' construct" 2 } */
  {
    ary[0] /* { dg-error "not specified in enclosing" } */
      = n; /* { dg-error "not specified in enclosing" } */
  }

#pragma acc data copy (ary, n)
  {
#pragma acc parallel default (none)
    {
      ary[0]
	= n;
    }

#pragma acc kernels default (none)
    {
      ary[0]
	= n;
    }
  }

  return 0;
}
