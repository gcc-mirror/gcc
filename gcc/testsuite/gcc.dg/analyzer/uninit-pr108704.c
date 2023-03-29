typedef unsigned short int __uint16_t;
typedef unsigned int __uint32_t;
typedef unsigned long int __uint64_t;
typedef __uint16_t uint16_t;
typedef __uint32_t uint32_t;
typedef __uint64_t uint64_t;

typedef uint32_t float32;
typedef struct
{
  uint64_t low;
  uint16_t high;
} floatx80;

extern floatx80
float32_to_floatx80(float32);

extern floatx80
floatx80_add(floatx80, floatx80);

floatx80
test (floatx80 a)
{
  floatx80 fp0;

  fp0 = a;
  fp0 = floatx80_add(fp0, float32_to_floatx80((0x3F800000))); /* { dg-bogus "use of uninitialized value 'fp0'" } */
  return fp0;
}
