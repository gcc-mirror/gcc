/* Test invalid use of host_data directive.  */

int v0;
#pragma acc host_data use_device(v0) /* { dg-error "expected declaration specifiers before" } */


void
f (void)
{
  int v2 = 3;
#pragma acc host_data copy(v2) /* { dg-error ".copy. is not valid for ..pragma acc host_data." } */
  ;

#pragma acc host_data use_device(v2)
  ;
  /* { dg-error ".use_device_ptr. variable is neither a pointer nor an array" "" { target c } 14 } */
  /* { dg-error ".use_device_ptr. variable is neither a pointer, nor an arraynor reference to pointer or array" "" { target c++ } 14 } */
  
#pragma acc host_data use_device(v0)
  ;
  /* { dg-error ".use_device_ptr. variable is neither a pointer nor an array" "" { target c } 19 } */
  /* { dg-error ".use_device_ptr. variable is neither a pointer, nor an arraynor reference to pointer or array" "" { target c++ } 19 } */
}


void
f2 (void)
{
  int x[100];

#pragma acc enter data copyin (x)
  /* Specifying an array index is not valid for host_data/use_device.  */
#pragma acc host_data use_device (x[4]) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  ;
#pragma acc exit data delete (x)
}


void
f3 (void)
{
  int x[100];

#pragma acc data copyin (x[25:50])
  {
    int *xp;
#pragma acc host_data use_device (x)
    {
      /* This use of the present clause is undefined behavior for OpenACC.  */
#pragma acc parallel present (x) copyout (xp) /* { dg-error "variable .x. declared in enclosing .host_data. region" } */
      {
        xp = x;
      }
    }
  }
}


void
f4 (void)
{
  int x[50];

#pragma acc data copyin (x[10:30])
  {
    int *xp;
#pragma acc host_data use_device (x)
    {
      /* Here 'x' being implicitly firstprivate for the parallel region
	 conflicts with it being declared as use_device in the enclosing
	 host_data region.  */
#pragma acc parallel copyout (xp)
      {
        xp = x; /* { dg-error "variable .x. declared in enclosing .host_data. region" } */
      }
    }
  }
}
