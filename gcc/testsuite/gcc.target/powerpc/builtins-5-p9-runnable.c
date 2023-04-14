/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#include <stdint.h>
#include <stdio.h> 
#include <inttypes.h>
#include <altivec.h> // vector

#define TRUE 1
#define FALSE 0

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

int result_wrong(vector unsigned char vec_expected,
		  vector unsigned char vec_actual)
{
  int i;

  for (i=0; i<16; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

int main() {
   int i, j;
   size_t size;
   unsigned char data_uc[100];
   vector unsigned char store_data_uc;
   unsigned char *address;
   vector unsigned char *datap;
   
   vector unsigned char vec_uc_expected1, vec_uc_result1;
   vector int data_int;
   
   for (i=0; i<100; i++)
      data_uc[i] = i+1;

   
   /* VEC_XL_LEN */
   
   size = 8;
   vec_uc_result1 = vec_xl_len (data_uc, size);

   vec_uc_expected1 = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8,
                                              0, 0, 0, 0, 0, 0, 0, 0};
   
   if (result_wrong (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: result does not match expected result\n");
       printf("vec_xl_len (%d): vec_uc_expected1[0] to vec_uc_expected1[15]\n",
	      size);

       for (i=0; i<16; i++)
	 printf(" %d,",vec_uc_expected1[i]);

       printf("\nvec_xl_len (%d): vec_uc_result1[0] to vec_uc_result1[15]\n",
	      size);
   
       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_result1[i]);

       printf("\n\n");
#else
       abort();
#endif
     }


   /* VEC_XL_LEN_R */
   size = 8;
   vec_uc_result1 = vec_xl_len_r(data_uc, size);

#ifdef __LITTLE_ENDIAN__
   vec_uc_expected1 = (vector unsigned char){8, 7, 6, 5, 4, 3, 2, 1,
					     0, 0, 0, 0, 0, 0, 0, 0,};
#else
   vec_uc_expected1 = (vector unsigned char){0, 0, 0, 0, 0, 0, 0, 0,
					     1, 2, 3, 4, 5, 6, 7, 8,};
#endif
   
   if (result_wrong (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: result does not match expected result\n");
       printf("vec_xl_len_r(%d): vec_uc_expected1[0] to vec_uc_expected1[15]\n",
	  size);
   
       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_expected1[i]);

       printf("\nvec_xl_len_r(%d): vec_uc_result1[0] to vec_uc_result1[15]\n",
	      size);
   
       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_result1[i]);

       printf("\n\n");
#else
       abort();
#endif
     }
       

   size = 4;
   vec_uc_result1 = vec_xl_len_r(data_uc, size);

#ifdef __LITTLE_ENDIAN__
   vec_uc_expected1 = (vector unsigned char){ 4, 3, 2, 1, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0 };
#else
   vec_uc_expected1 = (vector unsigned char){ 0, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 1, 2, 3, 4 };
#endif
   
   if (result_wrong (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: result does not match expected result\n");
       printf("vec_xl_len_r(%d): vec_uc_expected1[0] to vec_uc_expected1[15]\n",
	    size);
   
       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_expected1[i]);

       printf("\nvec_xl_len_r(%d): vec_uc_result1[0] to vec_uc_result1[15]\n",
	      size);
   
       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_result1[i]);

       printf("\n\n");
#else
       abort();
#endif
     }

   size = 2;
   vec_uc_result1 = vec_xl_len_r(data_uc, size);

#ifdef __LITTLE_ENDIAN__
   vec_uc_expected1 = (vector unsigned char){ 2, 1, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0 };
#else
   vec_uc_expected1 = (vector unsigned char){ 0, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 1, 2 };
#endif
   
   if (result_wrong (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: result does not match expected result\n");
       printf("vec_xl_len_r(%d): vec_uc_expected1[0] to vec_uc_expected1[15]\n",
	      size);
       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_expected1[i]);

       printf("\nvec_xl_len_r(%d) vec_uc_result1[0] to vec_uc_result1[15]\n",
	      size);
   
       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_result1[i]);

       printf("\n\n");
#else
       abort();
#endif
     }


   /* VEC_XST_LEN */
   vec_uc_expected1 = (vector unsigned char){ 1, 2, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0 };
   store_data_uc = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8,
					   9, 10, 11, 12, 13, 14, 15, 16 };
   size = 2;

   for (i=0; i<16; i++)
     vec_uc_result1[i] = 0;
   
   address = &vec_uc_result1[0];
   vec_xst_len (store_data_uc, address, size);

   if (result_wrong (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: result does not match expected result\n");
       printf("vec_xst_len (%d) vec_uc_result1[0] to vec_uc_result1[15]\n",
	      size);

       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_expected1[i]);

       printf("\nvec_xst_len (%d) store_data_uc[0] to store_data_uc[15]\n",
	      size);

       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_result1[i]);

       printf("\n\n");
#else
       abort();
#endif
     }

   vec_uc_expected1 = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8,
                                              9, 10, 11, 12, 13, 14, 0, 0 };
   store_data_uc = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8,
					   9, 10, 11, 12, 13, 14, 15, 16 };
   size = 14;

   for (i=0; i<16; i++)
     vec_uc_result1[i] = 0;

   address = &vec_uc_result1[0];

   vec_xst_len (store_data_uc, address, size);
   
   if (result_wrong (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: result does not match expected result\n");
       printf("vec_xst_len (%d) vec_uc_result1[0] to vec_uc_result1[15]\n",
	      size);

       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_expected1[i]);

       printf("\nvec_xst_len (%d) store_data_uc[0] to store_data_uc[15]\n",
	      size);

       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_result1[i]);

       printf("\n\n");
#else
       abort();
#endif
     }

   /* VEC_XST_LEN_R */
#ifdef __LITTLE_ENDIAN__
   vec_uc_expected1 = (vector unsigned char){ 16, 15, 14, 13, 12, 11, 10, 9,
					      8, 7, 6, 5, 4, 3, 2, 1 };
#else
   vec_uc_expected1 = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8,
					      9, 10, 11, 12, 13, 14, 15, 16 };
#endif
   store_data_uc = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8,
					   9, 10, 11, 12, 13, 14, 15, 16 };
   vec_uc_result1 = (vector unsigned char){ 0, 0, 0, 0, 0, 0, 0, 0,
					    0, 0, 0, 0, 0, 0, 0, 0 };

   size = 16;

   address = &vec_uc_result1[0];

   vec_xst_len_r(store_data_uc, address, size);

   if (result_wrong (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: result does not match expected result\n");
       printf("vec_xst_len_r(%d) vec_uc_expected1[0] to vec_uc_expected1[15]\n",
	      size);

       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_expected1[i]);

       printf("\nvec_xst_len_r(%d) result[0] to result[15]\n", size);

       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_result1[i]);

       printf("\n\n");
#else
       abort();
#endif
     }

#ifdef __LITTLE_ENDIAN__
   vec_uc_expected1 = (vector unsigned char){ 2, 1, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0 };
#else
   vec_uc_expected1 = (vector unsigned char){ 15, 16, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0 };
#endif
   store_data_uc = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8,
					   9, 10, 11, 12, 13, 14, 15, 16 };
   vec_uc_result1 = (vector unsigned char){ 0, 0, 0, 0, 0, 0, 0, 0, 
					    0, 0, 0, 0, 0, 0, 0, 0 };

   size = 2;

   address = &vec_uc_result1[0];

   vec_xst_len_r(store_data_uc, address, size);

   if (result_wrong (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: result does not match expected result\n");
       printf("vec_xst_len_r(%d) vec_uc_expected1[0] to vec_uc_expected1[15]\n",
	      size);
   
       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_expected1[i]);

       printf("\nvec_xst_len_r(%d) result[0] to result[15]\n", size);

       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_result1[i]);

       printf("\n\n");
#else
       abort();
#endif
     }

#ifdef __LITTLE_ENDIAN__
   vec_uc_expected1 = (vector unsigned char){ 16, 15, 14, 13, 12, 11, 10, 9,
                                              8, 7, 6, 5, 4, 3, 2, 1 };
#else
   vec_uc_expected1 = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8,
					      9, 10, 11, 12, 13, 14, 15, 16 };
#endif
   store_data_uc = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8,
					   9, 10, 11, 12, 13, 14, 15, 16 };
   vec_uc_result1 = (vector unsigned char){ 0, 0, 0, 0, 0, 0, 0, 0, 
					    0, 0, 0, 0, 0, 0, 0, 0 };

   size = 16;

   address = &vec_uc_result1[0];

   vec_xst_len_r(store_data_uc, address, size);

   if (result_wrong (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: result does not match expected result\n");
       printf("vec_xst_len_r(%d) vec_uc_expected1[0] to vec_uc_expected1[15]\n",
	  size);
   
       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_expected1[i]);

       printf("\nvec_xst_len_r(%d) result[0] to result[15]\n", size);

       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_result1[i]);

       printf("\n\n");
#else
       abort();
#endif
     }

#ifdef __LITTLE_ENDIAN__
   vec_uc_expected1 = (vector unsigned char){ 14, 13, 12, 11, 10, 9, 8, 7,
                                              6, 5, 4, 3, 2, 1, 0, 0 };
#else
   vec_uc_expected1 = (vector unsigned char){ 3, 4, 5, 6, 7, 8, 9, 10,
                                              11, 12, 13, 14, 15, 16, 0, 0 };
#endif
   store_data_uc = (vector unsigned char){ 1, 2, 3, 4, 5, 6, 7, 8,
					   9, 10, 11, 12, 13, 14, 15, 16 };
   vec_uc_result1 = (vector unsigned char){ 0, 0, 0, 0, 0, 0, 0, 0, 
					    0, 0, 0, 0, 0, 0, 0, 0 };

   size = 14;

   address = &vec_uc_result1[0];

   vec_xst_len_r(store_data_uc, address, size);

   if (result_wrong (vec_uc_expected1, vec_uc_result1))
     {
#ifdef DEBUG
       printf("Error: result does not match expected result\n");
       printf("vec_xst_len_r(%d) vec_uc_expected1[0] to vec_uc_expected1[15]\n",
	  size);
   
       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_expected1[i]);

       printf("\nvec_xst_len_r(%d) result[0] to result[15]\n", size);

       for (i=0; i<16; i++)
	 printf(" %d,", vec_uc_result1[i]);

       printf("\n\n");
#else
       abort();
#endif
     }
}
