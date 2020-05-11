/* { dg-do run } */
/* { dg-require-effective-target powerpc_future_hw } */
/* { dg-options "-mdejagnu-cpu=future" } */

#include <altivec.h>

extern void abort (void);

/* This test is replicated for every different vector type since
   vec_gnb is polymorphic.  */
unsigned long long int
do_vec_gnb (vector unsigned int source, int stride)
{
  switch (stride)
    {
    case 2:
      return vec_gnb (source, 2);
    case 3:
      return vec_gnb (source, 3);
    case 4:
      return vec_gnb (source, 4);
    case 5:
      return vec_gnb (source, 5);
    case 6:
      return vec_gnb (source, 6);
    case 7:
      return vec_gnb (source, 7);

    default:
      /* Illegal value of stride */
      abort ();
      return 0;
    }
}

int
main (int argc, char *argv [])
{
  /* For result = 0xaaaa_0000_0000_0000, use:
      stride = 2: binary 1x0x_1x0x_1x0x_... = 0x8888_8888_0000_0000, 0
      stride = 4: binary 1xxx_0xxx_1xxx_0xxxx = 0x8080_8080_8080_8080, 0

     For result = 0xaaaa_aaaa_0000_0000, use:
      stride = 2: source = 0x8888_8888_8888_8888, 0x0 }
      stride = 4: source = { 0x8080_8080_8080_8080, 0x8080_8080_8080_8080 }
  */

  /* The last array element appears in the left-most (first) bit
     positions of the vector register.  */
  vector unsigned int source_a = { 0, 0, 0, 0x88888888 };
  vector unsigned int source_b = { 0, 0, 0x80808080, 0x80808080 };
  vector unsigned int source_c = { 0, 0, 0x88888888, 0x88888888 };
  vector unsigned int source_d =
    { 0x80808080, 0x80808080, 0x80808080, 0x80808080 };

  unsigned long long int results [] =
    { 0xaaaa000000000000ull, 0xaaaa000000000000ull,
      0xaaaaaaaa00000000ull, 0xaaaaaaaa00000000ull };

  if (do_vec_gnb (source_a, 2) != results [0])
    abort ();
  if (do_vec_gnb (source_b, 4) != results [1])
    abort ();
  if (do_vec_gnb (source_c, 2) != results [2])
    abort ();
  if (do_vec_gnb (source_d, 4) != results [3])
    abort ();

  return 0;
}

