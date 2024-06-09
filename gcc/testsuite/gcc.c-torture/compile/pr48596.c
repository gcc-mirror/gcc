/* PR target/48596  */
/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

enum { nrrdCenterUnknown, nrrdCenterNode, nrrdCenterCell, nrrdCenterLast };
typedef struct { int size; int center; }  NrrdAxis;
typedef struct { int dim; NrrdAxis axis[10]; } Nrrd;
typedef struct { } NrrdKernel;
typedef struct { const NrrdKernel *kernel[10]; int samples[10]; } Info;
int _nrrdCenter(int);

void
foo (Nrrd *nout, Nrrd *nin, const NrrdKernel *kernel, const double *parm,
     const int *samples, const double *scalings)
{
  Info *info;
  int d, p, np, center;
  for (d=0; d<nin->dim; d++)
    {
      info->kernel[d] = kernel;
      if (samples)
	info->samples[d] = samples[d];
      else
	{
	  center = _nrrdCenter(nin->axis[d].center);
	  if (nrrdCenterCell == center)
	    info->samples[d] = nin->axis[d].size*scalings[d];
	  else
	    info->samples[d] = (nin->axis[d].size - 1)*scalings[d] + 1;
	}
    }
}
