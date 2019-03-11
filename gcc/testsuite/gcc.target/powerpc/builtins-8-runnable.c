/* { dg-do run { target { powerpc*-*-* && { p8vector_hw } } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

#include <stdint.h>
#include <stdio.h>
#include <math.h>
#include <altivec.h>

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

int main()
  {
    int i;
    vector float f_arg1;
    vector double d_arg1;
   
    vector float vec_f_expected1, vec_f_result1, vec_f_error;
    vector double vec_d_expected1, vec_d_result1, vec_d_error;
  
    /* vec_expte: float args, result */
    f_arg1 = (vector float){1.0, 2.0, 3.0, 4.0};
    vec_f_expected1 = (vector float){2.0, 4.0, 8.0, 16.0};

    vec_f_result1 = vec_expte (f_arg1);

    for (i = 0; i < 4; i++)
      {
        if (vec_f_expected1[i] != vec_f_result1[i])
#ifdef DEBUG
           printf("ERROR vec_expte (f) result[%d]=%f != expected[%d]=%f\n",
		  i, vec_f_result1[i],  i, vec_f_expected1[i]);
#else
        abort();
#endif
      }

    /* vec_loge: float args, result */
    f_arg1 = (vector float){4.0, 8.0, 16.0, 64};
    vec_f_expected1 = (vector float){2.0, 3.0, 4.0, 6.0};

    vec_f_result1 = vec_loge (f_arg1);

    for (i = 0; i < 4; i++)
      {
        if (vec_f_expected1[i] != vec_f_result1[i])
#ifdef DEBUG
	  printf("ERROR vec_loge (f) result[%d]=%f != expected[%d]=%f\n",
		 i, vec_f_result1[i],  i, vec_f_expected1[i]);
#else
          abort();
#endif
    }

    /* vec_re: float args, result  (calculate approximate reciprocal)  */
    f_arg1 = (vector float){1.0, 5.0, 4.0, 8.0};
    vec_f_expected1 = (vector float){1.0, 0.2, 0.25, 0.125};
    vec_f_error = (vector float){1.0, 0.2, 0.25, 0.125};

    vec_f_result1 = vec_re (f_arg1);
  
    for (i = 0; i < 4; i++)
      {
        vec_f_error[i] = fabs(vec_f_expected1[i] - vec_f_result1[i]);
  
        if (vec_f_error[i] >=  0.0001)
#ifdef DEBUG
           printf("ERROR vec_re (f) result[%d]=%f != expected[%d]=%f\n",
		  i, vec_f_result1[i],  i, vec_f_expected1[i]);
#else
	abort();
#endif
      }

    /* vec_re: double args, result  (calculate approximate reciprocal)  */
    d_arg1 = (vector double){1.0, 8.0};
    vec_d_expected1 = (vector double){1.0, 0.125};
    vec_d_error = (vector double){1.0, 0.125};

    vec_d_result1 = vec_re (d_arg1);
  
    for (i = 0; i < 2; i++)
      {
         vec_d_error[i] = fabs(vec_d_expected1[i] - vec_d_result1[i]);
  
         if (vec_d_error[i] >=  0.0001)
#ifdef DEBUG
           printf("ERROR vec_re (d) result[%d]=%f != expected[%d]=%f\n",
		  i, vec_d_result1[i],  i, vec_d_expected1[i]);
#else
          abort();
#endif
      }
  }
