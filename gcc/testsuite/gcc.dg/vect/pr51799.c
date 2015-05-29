/* { dg-do compile } */

typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef signed short int16_t;
typedef unsigned long uint32_t;
void
f0a (uint32_t * __restrict__ result, int8_t * __restrict__ arg1,
     uint32_t * __restrict__ arg4, int8_t temp_6)
{
  int idx;
  for (idx = 0; idx < 416; idx += 1)
    {
      result[idx] = (uint8_t)(((arg1[idx] << 7) + arg4[idx]) * temp_6);
    }
}

