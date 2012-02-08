/* { dg-do compile } */

typedef unsigned char uint8_t ;
typedef unsigned long uint32_t;
void f0a(uint32_t * result, uint32_t * arg1)
{
  int idx;
  for (idx=0;idx<96;idx += 1)
    {
      uint8_t temp_5;
      uint8_t temp_6;

      temp_5 = ~(*arg1);
      temp_6 = (*arg1) + 1 - temp_5;
      result[idx] = temp_6;
    }
}
