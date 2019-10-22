#include <arm_neon.h>

typedef int16x4_t myvec;

void f (float x)
{
  __Int8x8_t y1 = x; // { dg-error {cannot convert 'float' to '__Int8x8_t' in initialization} }
  __Int8x8_t *ptr1 = &x; // { dg-error {cannot convert 'float\*' to '__Int8x8_t\*' in initialization} }
  int8x8_t y2 = x; // { dg-error {cannot convert 'float' to 'int8x8_t' in initialization} }
  int8x8_t *ptr2 = &x; // { dg-error {cannot convert 'float\*' to 'int8x8_t\*' in initialization} }
  myvec y3 = x; // { dg-error {cannot convert 'float' to 'myvec' {aka 'int16x4_t'} in initialization} }
  myvec *ptr3 = &x; // { dg-error {cannot convert 'float\*' to 'myvec\*' {aka 'int16x4_t\*'} in initialization} }
}
