/* { dg-do run } */
/* { dg-options "-O2 -fno-inline --save-temps" } */

extern void abort (void);

typedef float float32x2_t __attribute__ ((__vector_size__ ((8))));
typedef unsigned int uint32x2_t __attribute__ ((__vector_size__ ((8))));

float32x2_t
test_dup_1 (float32x2_t in)
{
  return __builtin_shuffle (in, (uint32x2_t) {1, 1});
}

int
main (int argc, char **argv)
{
  float32x2_t test = {2.718, 3.141};
  float32x2_t res = test_dup_1 (test);
  if (res[0] != test[1] || res[1] != test[1])
    abort ();
  return 0;
}

/* { dg-final { scan-assembler-times "\[ \t\]*dup\[ \t\]+v\[0-9\]+\.2s, ?v\[0-9\]+\.s\\\[\[01\]\\\]" 1 } } */
/* { dg-final { scan-assembler-not "zip" } } */
/* { dg-final { cleanup-saved-temps } } */

