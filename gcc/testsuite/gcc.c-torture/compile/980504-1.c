typedef struct _geom_elem {
  double        coeffs[6];
} pGeomDefRec, *pGeomDefPtr;
typedef struct _mpgeombanddef {
	int	yOut;		 
	int	in_width;	 
} mpGeometryBandRec, *mpGeometryBandPtr;
typedef void *pointer;
typedef unsigned char  CARD8;
typedef CARD8 BytePixel;
void  BiGL_B  (OUTP,srcimg,width,sline,pedpvt,pvtband)	pointer OUTP;
pointer *srcimg;
register int width;
int sline;
pGeomDefPtr pedpvt; mpGeometryBandPtr pvtband;
{
  register float s, t, st;
  register int 	isrcline,isrcpix;
  register int 	srcwidth = pvtband->in_width - 1;
  register   BytePixel  val;
  register   BytePixel  *ptrIn, *ptrJn;
  register double a  = pedpvt->coeffs[0];
  register double c  = pedpvt->coeffs[2];
  register double srcpix  = a * ((double)(0.0000))  +	pedpvt->coeffs[1] * (pvtband->yOut + ((double)(0.0000)) ) +	pedpvt->coeffs[4];
  register double srcline = c * ((double)(0.0000))  +	pedpvt->coeffs[3] * (pvtband->yOut + ((double)(0.0000)) ) +	pedpvt->coeffs[5];
  if ( (isrcpix >= 0) && (isrcpix < srcwidth) )
    val =	ptrIn[isrcpix]   * ((float)1. - s - t + st) + ptrIn[isrcpix+1] * (s - st) +	ptrJn[isrcpix]   * (t - st) +	ptrJn[isrcpix+1] * (st) +   (float)0.5 ;
} 
