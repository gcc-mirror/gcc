/* { dg-require-effective-target alloca } */
typedef const int cint;
typedef struct {
} Bounds;
int ndim_, ncomp_, selectedcomp_, nregions_;
void *voidregion_;
typedef struct {
    double diff, err, spread;
} Errors;
typedef const Errors cErrors;
void Split(int iregion, int depth, int xregion)
{
  typedef struct {
      double avg, err, spread, chisq;
      double xmin[ndim_], xmax[ndim_];
  } Result;
  typedef struct region {
      Result result[ncomp_];
  } Region;
  Errors errors[ncomp_];
  int comp, ireg, xreg;
  for( ireg = iregion, xreg = xregion; ireg < nregions_; ireg = xreg++ )
    {
      Result *result = ((Region *)voidregion_)[ireg].result;
      for( comp = 0; comp < ncomp_; ++comp )
	{
	  Result *r = &result[comp];
	  cErrors *e = &errors[comp];
	  double c = e->diff;
	  if( r->err > 0 ) r->err = r->err*e->err + c;
	}
    }
}

