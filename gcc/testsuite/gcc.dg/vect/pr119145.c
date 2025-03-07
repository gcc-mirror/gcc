/* { dg-do compile } */

typedef short Quantum;
Quantum ComplexImages_Bi_0, ComplexImages_Ai_0, ComplexImages_Ai_1;
long ComplexImages_x;
__attribute__((__simd__)) double atan2(double, double);
typedef enum { MagickFalse } MagickBooleanType;

struct {
    MagickBooleanType matte;
} *ComplexImages_images;

typedef struct {
    Quantum blue, opacity;
} PixelPacket;

typedef enum { MagnitudePhaseComplexOperator } ComplexOperator;
PixelPacket ComplexImages_Ar, ComplexImages_Br;
PixelPacket *ComplexImages_Ci;
ComplexOperator ComplexImages_op;

void ComplexImages()
{
  for (; ComplexImages_x; ComplexImages_x++)
    switch (ComplexImages_op)
      {
      case MagnitudePhaseComplexOperator:
        if (ComplexImages_images->matte)
	  ComplexImages_Ci->opacity
	    = atan2(ComplexImages_Ai_1, ComplexImages_Ar.opacity);
	ComplexImages_Ci->blue
	  = 1.0 / (ComplexImages_Ai_0 * ComplexImages_Br.blue
		   + ComplexImages_Ar.blue * ComplexImages_Bi_0);
      }
}
