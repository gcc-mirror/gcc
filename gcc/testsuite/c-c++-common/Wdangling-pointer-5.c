/* PR middle-end/63272 - GCC should warn when using pointer to dead scoped
   variable within the same function
   Exercise -Wdangling-pointer for escaping stores of addreses of auto
   variables.
   { dg-do compile }
   { dg-options "-O0 -Wall -ftrack-macro-expansion=0" } */

void* alloca (__SIZE_TYPE__);

void* sink (void*, ...);

extern void *evp;

void nowarn_store_extern_call (void)
{
  int x;
  evp = &x;
  sink (0);
}

void nowarn_store_extern_ovrwrite (void)
{
  int x;
  evp = &x;
  evp = 0;
}

void nowarn_store_extern_store (void)
{
  int x;
  void **p = (void**)sink (&evp);
  evp = &x;
  *p = 0;
}


void warn_store_alloca (int n)
{
  // This fails because of a bug in the warning.
  void *p = alloca (n);
  evp = p;           // { dg-warning "storing the address of local variable 'x' in 'evp1'" "pr??????" { xfail *-*-* } }
}


void warn_store_extern (void)
{
  extern void *evp1;  // { dg-message "'evp1' declared here" }
  int x;              // { dg-message "'x' declared here" }
  evp1 = &x;          // { dg-warning "storing the address of local variable 'x' in 'evp1'" }
}


void nowarn_store_arg_call (void **vpp)
{
  int x;
  *vpp = &x;
  sink (0);
}

void nowarn_store_arg_ovrwrite (void **vpp)
{
  int x;
  *vpp = &x;
  *vpp = 0;
}

void nowarn_store_arg_store (void **vpp)
{
  int x;
  void **p = (void**)sink (0);
  *vpp = &x;
  *p = 0;
}

void* nowarn_store_arg_store_arg (void **vpp1, void **vpp2)
{
  int x;              // { dg-message "'x' declared here" }
  void **p = (void**)sink (0);
  *vpp1 = &x;         // { dg-warning "storing the address of local variable 'x' in '\\*vpp1'" }
  *vpp2 = 0;          // might overwrite *vpp1
  return p;
}

void warn_store_arg (void **vpp)
{
  int x;              // { dg-message "'x' declared here" }
  *vpp = &x;          // { dg-warning "storing the address of local variable 'x' in '\\*vpp'" }
}


