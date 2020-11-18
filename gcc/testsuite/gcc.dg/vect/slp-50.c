/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */

typedef int Quantum;
typedef struct {
  Quantum blue, green;
} PixelPacket;
PixelPacket *EnhanceImage_image_q;
int EnhanceImage_image_x;
float EnhanceImage_image_distance_squared_total_weight;
void EnhanceImage_image_distance_squared()
{
  float zero_1;
  for (; EnhanceImage_image_x; EnhanceImage_image_x++) {
      EnhanceImage_image_distance_squared_total_weight += 5.0;
      EnhanceImage_image_q->green = EnhanceImage_image_q->blue =
	  zero_1 + EnhanceImage_image_distance_squared_total_weight / 2 - 1;
      EnhanceImage_image_q++;
  }
}
