static const double internalEndianMagic = 7.949928895127363e-275;
static const unsigned char ieee_754_mantissa_mask[] = { 0x00, 0x0F, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF };
static inline int 
trio_isnan (double number)
{
  int has_mantissa = 0;
  unsigned int i;
  unsigned char current;
  for (i = 0; i < (unsigned int)sizeof(double); i++)
    {
      current = ((unsigned char *)&number)[(((unsigned char
					      *)&internalEndianMagic)[7-(i)])];
      has_mantissa |= (current & ieee_754_mantissa_mask[i]);
    }
  return has_mantissa;
}
void
xmlXPathEqualNodeSetFloat(int nodeNr, double v)
{
  int i;
  for (i=0; i<nodeNr; i++)
    if (!trio_isnan(v))
      break;
}

