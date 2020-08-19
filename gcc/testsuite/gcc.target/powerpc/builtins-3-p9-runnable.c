/* { dg-do run } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#ifdef DEBUG
#include <stdio.h>
#endif

#include <altivec.h> // vector

void abort (void);

int main() {
   int i;
   vector float vfr, vfexpt;
   vector unsigned short vusha;

   /* 1.0, -2.0, 0.0, 8.5, 1.5, 0.5, 1.25, -0.25 */
   vusha = (vector unsigned short){0B011110000000000, 0B1100000000000000,
                                   0B000000000000000, 0B0100100001000000,
                                   0B011111000000000, 0B0011100000000000,
                                   0B011110100000000, 0B1011010000000000};

#ifdef DEBUG
   printf ("Claim, source data is 8 16-bit floats:\n");
   printf ("   {1.0, -2.0, 0.0, 8.5, 1.5, 0.5, 1.25, -0.25}\n");
   printf ("vusha = (vector unsigned short){0B011110000000000, 0B1100000000000000,\n");
   printf ("                                0B000000000000000, 0B0100100001000000,\n");
   printf ("                                0B011111000000000, 0B0011100000000000,\n");
   printf ("                                0B011110100000000, 0B1011010000000000};\n\n");
#endif

   /* The ABI lists the builtins as:

        vec_extract_fp32_from_shorth()
        vec_extract_fp32_from_shortl()

      GCC will also accept and map the builtin names

        vec_extract_fp_from_shorth()
        vec_extract_fp_from_shortl()

      to the same builtins internally.  For completeness,
      test both builtin function names.  */

   vfexpt = (vector float){1.0, -2.0, 0.0, 8.5};
   vfr = vec_extract_fp32_from_shorth(vusha);

#ifdef DEBUG
   printf ("vec_extract_fp32_from_shorth\n");
   for (i=0; i<4; i++)
     printf("result[%d] = %f; expected[%d] = %f\n",
	    i, vfr[i], i, vfexpt[i]);
#endif

   for (i=0; i<4; i++) {
      if (vfr[i] != vfexpt[i])
         abort();
   }

   vfexpt = (vector float){1.5, 0.5, 1.25, -0.25};
   vfr = vec_extract_fp32_from_shortl(vusha);

#ifdef DEBUG
   printf ("\nvec_extract_fp32_from_shortl\n");
   for (i=0; i<4; i++)
     printf("result[%d] = %f; expected[%d] = %f\n",
	    i, vfr[i], i, vfexpt[i]);
#endif

    for (i=0; i<4; i++) {
      if (vfr[i] != vfexpt[i])
         abort();
   }
   vfexpt = (vector float){1.0, -2.0, 0.0, 8.5};
   vfr = vec_extract_fp_from_shorth(vusha);

#ifdef DEBUG
   printf ("vec_extract_fp_from_shorth\n");
   for (i=0; i<4; i++)
     printf("result[%d] = %f; expected[%d] = %f\n",
	    i, vfr[i], i, vfexpt[i]);
#endif

   for (i=0; i<4; i++) {
      if (vfr[i] != vfexpt[i])
         abort();
   }

   vfexpt = (vector float){1.5, 0.5, 1.25, -0.25};
   vfr = vec_extract_fp_from_shortl(vusha);

#ifdef DEBUG
   printf ("\nvec_extract_fp_from_shortl\n");
   for (i=0; i<4; i++)
     printf("result[%d] = %f; expected[%d] = %f\n",
	    i, vfr[i], i, vfexpt[i]);
#endif

    for (i=0; i<4; i++) {
      if (vfr[i] != vfexpt[i])
         abort();
   }
}
