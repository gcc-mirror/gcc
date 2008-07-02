/* Used by proc_ptr_8.f90.
   PR fortran/32580.  */

int (*funpointer)(int);

int f(int t)
{
  return t*3;
}

void init()
{
 funpointer=f;
}
