/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

static void addPrior(float center_x, float center_y, float width, float height,
                     bool normalized, float *dst)
{
  if (normalized)
    {
      dst[0] = (center_x - width * 0.5f);
      dst[1] = (center_y - height * 0.5f);
      dst[2] = (center_x + width * 0.5f);
      dst[3] = (center_y + height * 0.5f);
    }
  else
    {
      dst[0] = center_x - width * 0.5f;
      dst[1] = center_y - height * 0.5f;
      dst[2] = center_x + width * 0.5f - 1.0f;
      dst[3] = center_y + height * 0.5f - 1.0f;
    }
}
void forward(float *outputPtr, int _offsetsXs, float *_offsetsX,
	     float *_offsetsY, float _stepX, float _stepY,
	     bool _bboxesNormalized, float _boxWidth, float _boxHeight)
{
  for (int j = 0; j < _offsetsXs; ++j)
    {
      float center_x = (_offsetsX[j]) * _stepX;
      float center_y = (_offsetsY[j]) * _stepY;
      addPrior(center_x, center_y, _boxWidth, _boxHeight, _bboxesNormalized,
	       outputPtr);
      outputPtr += 4;
    }
}
