/* This used to ICE due to NRVO being attempted on 's', NRVO can not be
   done on a variable used in an allocate directive.  */

/* NRVO probably kicks in when sizeof(T) is greater than 16.  */
struct S {
  char _v[17];
};

S f0()
{
  S s;
  #pragma omp allocate(s)
  return s;
}

/* Also test with a VERY LARGE type just in case the above isn't big enough
   to trigger NRVO in all cases.  */
struct Big {
  char _v[4096];
};

Big f1()
{
  Big b;
  #pragma omp allocate(b)
  return b;
}
