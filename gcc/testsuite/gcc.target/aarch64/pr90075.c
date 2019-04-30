/* { dg-do compile } */
/* { dg-additional-options "-O1" } */

typedef struct {
  float one, two;
} twofloats;

float
bug (twofloats tf)
{
  float f1, f2;
  union {
    twofloats tfloats;
    float arr[2];
  } utfloats;

  utfloats.tfloats = tf;
  f1 = utfloats.arr[1];
  f2 = __builtin_copysignf (0, f1);
  return f2;
}
