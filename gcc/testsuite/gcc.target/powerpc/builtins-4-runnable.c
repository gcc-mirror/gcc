/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-maltivec -mvsx" } */

#include <inttypes.h>
#include <altivec.h> // vector

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

int main() {
  int i;
  signed char data_c[100];
  unsigned char data_uc[100];

  signed short int data_ssi[100];
  unsigned short int data_usi[100];

  signed int data_si[100];
  unsigned int data_ui[100];

  signed long long data_sll[100];
  unsigned long long data_ull[100];

  float data_f[100];
  double data_d[100];
  signed long long disp;

  vector signed char vec_c_expected1, vec_c_expected2, vec_c_result1, vec_c_result2;
  vector unsigned char vec_uc_expected1, vec_uc_expected2,
    vec_uc_result1, vec_uc_result2;
  vector signed short int vec_ssi_expected1, vec_ssi_expected2,
    vec_ssi_result1, vec_ssi_result2;
  vector unsigned short int  vec_usi_expected1, vec_usi_expected2,
    vec_usi_result1, vec_usi_result2;
  vector signed int vec_si_expected1, vec_si_expected2, vec_si_result1,
    vec_si_result2;
  vector unsigned int vec_ui_expected1, vec_ui_expected2, vec_ui_result1,
    vec_ui_result2;
  vector signed long long vec_sll_expected1, vec_sll_expected2,
    vec_sll_result1, vec_sll_result2;
  vector unsigned long long vec_ull_expected1, vec_ull_expected2,
    vec_ull_result1, vec_ull_result2;
  vector float vec_f_expected1, vec_f_expected2, vec_f_result1, vec_f_result2;
  vector double vec_d_expected1, vec_d_expected2, vec_d_result1, vec_d_result2;
  char buf[20];
  signed long long zero = (signed long long) 0;

  for (i = 0; i < 100; i++)
    {
      data_c[i] = i;
      data_uc[i] = i+1;
      data_ssi[i] = i+10;
      data_usi[i] = i+11;
      data_si[i] = i+100;
      data_ui[i] = i+101;
      data_sll[i] = i+1000;
      data_ull[i] = i+1001;
      data_f[i] = i+100000.0;
      data_d[i] = i+1000000.0;
    }

  // vec_xl() tests
  disp = 1;

  vec_c_expected1 = (vector signed char){0, 1, 2, 3, 4, 5, 6, 7,
					 8, 9, 10, 11, 12, 13, 14, 15};
  vec_c_result1 = vec_xl (0, data_c);

  vec_c_expected2 = (vector signed char){1, 2, 3, 4, 5, 6, 7, 8, 9,
					 10, 11, 12, 13, 14, 15, 16};
  vec_c_result2 = vec_xl (disp, data_c);

  vec_uc_expected1 = (vector unsigned char){1, 2, 3, 4, 5, 6, 7, 8, 9,
					    10, 11, 12, 13, 14, 15, 16};
  vec_uc_result1 = vec_xl (0, data_uc);

  vec_uc_expected2 = (vector unsigned char){2, 3, 4, 5, 6, 7, 8, 9, 10,
					    11, 12, 13, 14, 15, 16, 17};
  vec_uc_result2 = vec_xl (disp, data_uc);

  for (i = 0; i < 16; i++)
    {
      if (vec_c_result1[i] != vec_c_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_c_result1[%d] = %d; vec_c_expected1[%d] = %d\n",
	       i,  vec_c_result1[i], i, vec_c_expected1[i]);
#else
	abort ();
#endif
      if (vec_c_result2[i] != vec_c_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_c_result2[%d] = %d; vec_c_expected2[%d] = %d\n",
	       i,  vec_c_result2[i], i, vec_c_expected2[i]);
#else
	abort ();
#endif

      if (vec_uc_result1[i] != vec_uc_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_uc_result1[%d] = %d; vec_uc_expected1[%d] = %d\n",
	       i,  vec_uc_result1[i], i, vec_uc_expected1[i]);
#else
	abort ();
#endif

      if (vec_uc_result2[i] != vec_uc_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_uc_result2[%d] = %d; vec_uc_expected2[%d] = %d\n",
	       i,  vec_uc_result2[i], i, vec_uc_expected2[i]);
#else
	abort ();
#endif
    }

  disp = 2;
  vec_ssi_expected1 = (vector signed short){10, 11, 12, 13, 14, 15, 16, 17};

  vec_ssi_result1 = vec_xl (0, data_ssi);

  vec_ssi_expected2 = (vector signed short){11, 12, 13, 14, 15, 16, 17, 18};
  vec_ssi_result2 = vec_xl (disp, data_ssi);

  vec_usi_expected1 = (vector unsigned short){11, 12, 13, 14, 15, 16, 17, 18};
  vec_usi_result1 = vec_xl (0, data_usi);

  vec_usi_expected2 = (vector unsigned short){12, 13, 14, 15, 16, 17, 18, 19};
  vec_usi_result2 = vec_xl (disp, data_usi);


  for (i = 0; i < 8; i++)
    {
      if (vec_ssi_result1[i] != vec_ssi_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_ssi_result1[%d] = %d; vec_ssi_expected1[%d] = %d\n",
	       i,  vec_ssi_result1[i], i, vec_ssi_expected1[i]);
#else
	abort ();
#endif
      if (vec_ssi_result2[i] != vec_ssi_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_ssi_result2[%d] = %d; vec_ssi_expected2[%d] = %d\n",
	       i,  vec_ssi_result2[i], i, vec_ssi_expected2[i]);
#else
	abort ();
#endif

      if (vec_usi_result1[i] != vec_usi_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_usi_result1[%d] = %d; vec_usi_expected1[%d] = %d\n",
	       i,  vec_usi_result1[i], i, vec_usi_expected1[i]);
#else
	abort ();
#endif

      if (vec_usi_result2[i] != vec_usi_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_usi_result2[%d] = %d; vec_usi_expected2[%d] = %d\n",
	       i,  vec_usi_result2[i], i, vec_usi_expected2[i]);
#else
	abort ();
#endif
    }

  disp = 4;
  vec_si_result1 = vec_xl (zero, data_si);
  vec_si_expected1 = (vector int){100, 101, 102, 103};

  vec_si_result2 = vec_xl (disp, data_si);
  vec_si_expected2 = (vector int){101, 102, 103, 104};

  vec_ui_result1 = vec_xl (zero, data_ui);
  vec_ui_expected1 = (vector unsigned int){101, 102, 103, 104};

  vec_ui_result2 = vec_xl (disp, data_ui);
  vec_ui_expected2 = (vector unsigned int){102, 103, 104, 105};

  for (i = 0; i < 4; i++)
    {
      if (vec_si_result1[i] != vec_si_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_si_result1[%d] = %d; vec_si_expected1[%d] = %d\n",
	       i,  vec_si_result1[i], i, vec_si_expected1[i]);
#else
	abort ();
#endif
      if (vec_si_result2[i] != vec_si_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_si_result2[%d] = %d; vec_si_expected2[%d] = %d\n",
	       i,  vec_si_result2[i], i, vec_si_expected2[i]);
#else
	abort ();
#endif

      if (vec_ui_result1[i] != vec_ui_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_ui_result1[%d] = %d; vec_ui_expected1[%d] = %d\n",
	       i,  vec_ui_result1[i], i, vec_ui_expected1[i]);
#else
	abort ();
#endif

      if (vec_ui_result2[i] != vec_ui_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_ui_result2[%d] = %d; vec_ui_expected1[%d] = %d\n",
	       i,  vec_si_result2[i], i, vec_ui_expected1[i]);
#else
	abort ();
#endif
    }

  disp = 8;
  vec_sll_result1 = vec_xl (zero, data_sll);
  vec_sll_expected1 = (vector signed long long){1000, 1001};

  vec_sll_result2 = vec_xl (disp, data_sll);
  vec_sll_expected2 = (vector signed long long){1001, 1002};

  vec_ull_result1 = vec_xl (zero, data_ull);
  vec_ull_expected1 = (vector unsigned long long){1001, 1002};

  vec_ull_result2 = vec_xl (disp, data_ull);
  vec_ull_expected2 = (vector unsigned long long){1002, 1003};

  for (i = 0; i < 2; i++)
    {
      if (vec_sll_result1[i] != vec_sll_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_sll_result1[%d] = %lld; vec_sll_expected1[%d] = %lld\n",
	       i,  vec_sll_result1[i], i, vec_sll_expected1[i]);
#else
	abort ();
#endif

      if (vec_sll_result2[i] != vec_sll_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_sll_result2[%d] = %lld; vec_sll_expected2[%d] = %lld\n",
	       i,  vec_sll_result2[i], i, vec_sll_expected2[i]);
#else
	abort ();
#endif

      if (vec_ull_result1[i] != vec_ull_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_ull_result1[%d] = %lld; vec_ull_expected1[%d] = %lld\n",
	       i,  vec_ull_result1[i], i, vec_ull_expected1[i]);
#else
	abort ();
#endif

      if (vec_ull_result2[i] != vec_ull_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_ull_result2[%d] = %lld; vec_ull_expected2[%d] = %lld\n",
	       i,  vec_ull_result2[i], i, vec_ull_expected2[i]);
#else
	abort ();
#endif
    }

  disp = 4;
  vec_f_result1 = vec_xl (zero, data_f);
  vec_f_expected1 = (vector float){100000.0, 100001.0, 100002.0, 100003.0};

  vec_f_result2 = vec_xl (disp, data_f);
  vec_f_expected2 = (vector float){100001.0, 100002.0, 100003.0, 100004.0};

  for (i = 0; i < 4; i++)
    {
      if (vec_f_result1[i] != vec_f_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_f_result1[%d] = %f; vec_f_expected1[%d] = %f\n",
	       i,  vec_f_result1[i], i, vec_f_expected1[i]);
#else
	abort ();
#endif

      if (vec_f_result2[i] != vec_f_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_f_result2[%d] = %f; vec_f_expected2[%d] = %f\n",
	       i,  vec_f_result2[i], i, vec_f_expected2[i]);
#else
	abort ();
#endif
    }

  disp = 8;
  vec_d_result1 = vec_xl (zero, data_d);
  vec_d_expected1 = (vector double){1000000.0, 1000001.0};

  vec_d_result2 = vec_xl (disp, data_d);
  vec_d_expected2 = (vector double){1000001.0, 1000002.0};

  for (i = 0; i < 2; i++)
    {
      if (vec_d_result1[i] != vec_d_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_d_result1[%d] = %f; vec_f_expected1[%d] = %f\n",
	       i,  vec_d_result1[i], i, vec_d_expected1[i]);
#else
	abort ();
#endif

      if (vec_d_result2[i] != vec_d_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl(), vec_d_result2[%d] = %f; vec_f_expected2[%d] = %f\n",
	       i,  vec_d_result2[i], i, vec_d_expected2[i]);
#else
	abort ();
#endif
    }

  // vec_xl_be() tests
  disp = 1;
#ifdef __BIG_ENDIAN__
  vec_c_expected1 = (vector signed char){0, 1, 2, 3, 4, 5, 6, 7,
					 8, 9, 10, 11, 12, 13, 14, 15};
#else
  vec_c_expected1 = (vector signed char){15, 14, 13, 12, 11, 10, 9, 8,
					 7, 6, 5, 4, 3, 2, 1, 0};
#endif
  vec_c_result1 = vec_xl_be (0, data_c);



#ifdef __BIG_ENDIAN__
  vec_c_expected2 = (vector signed char){1, 2, 3, 4, 5, 6, 7, 8,
					 9, 10, 11, 12, 13, 14, 15, 16};
#else
  vec_c_expected2 = (vector signed char){16, 15, 14, 13, 12, 11, 10, 9,
					 8, 7, 6, 5, 4, 3, 2, 1};
#endif

  vec_c_result2 = vec_xl_be (disp, data_c);

#ifdef __BIG_ENDIAN__
  vec_uc_expected1 = (vector unsigned char){1, 2, 3, 4, 5, 6, 7, 8,
					    9, 10, 11, 12, 13, 14, 15, 16};
#else
  vec_uc_expected1 = (vector unsigned char){16, 15, 14, 13, 12, 11, 10, 9,
					 8, 7, 6, 5, 4, 3, 2, 1};
#endif

  vec_uc_result1 = vec_xl_be (0, data_uc);

#ifdef __BIG_ENDIAN__
  vec_uc_expected2 = (vector unsigned char){2, 3, 4, 5, 6, 7, 8, 9,
					    10, 11, 12, 13, 14, 15, 16, 17};
#else
  vec_uc_expected2 = (vector unsigned char){17, 16, 15, 14, 13, 12, 11, 10,
					    9, 8, 7, 6, 5, 4, 3, 2};
#endif

  vec_uc_result2 = vec_xl_be (disp, data_uc);

  for (i = 0; i < 16; i++)
    {
      if (vec_c_result1[i] != vec_c_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_c_result1[%d] = %d; vec_c_expected1[%d] = %d\n",
	       i,  vec_c_result1[i], i, vec_c_expected1[i]);
#else
	abort ();
#endif

      if (vec_c_result2[i] != vec_c_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_c_result2[%d] = %d; vec_c_expected2[%d] = %d\n",
	       i,  vec_c_result2[i], i, vec_c_expected2[i]);
#else
	abort ();
#endif

      if (vec_uc_result1[i] != vec_uc_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_uc_result1[%d] = %d; vec_uc_expected1[%d] = %d\n",
	       i,  vec_uc_result1[i], i, vec_uc_expected1[i]);
#else
	abort ();
#endif

      if (vec_uc_result2[i] != vec_uc_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_uc_result2[%d] = %d; vec_uc_expected2[%d] = %d\n",
	       i,  vec_uc_result1[i], i, vec_uc_expected1[i]);
#else
	abort ();
#endif
    }

  vec_ssi_result1 = vec_xl_be (zero, data_ssi);

#ifdef __BIG_ENDIAN__
  vec_ssi_expected1 = (vector signed short){10, 11, 12, 13, 14, 15, 16, 17};
#else
  vec_ssi_expected1 = (vector signed short){17, 16, 15, 14, 13, 12, 11, 10};
#endif

  disp = 2;
  vec_ssi_result2 = vec_xl_be (disp, data_ssi);

#ifdef __BIG_ENDIAN__
  vec_ssi_expected2 = (vector signed short){11, 12, 13, 14, 15, 16, 17, 18};
#else
  vec_ssi_expected2 = (vector signed short){18, 17, 16, 15, 14, 13, 12, 11};
#endif

  vec_usi_result1 = vec_xl_be (zero, data_usi);

#ifdef __BIG_ENDIAN__
  vec_usi_expected1 = (vector unsigned short){11, 12, 13, 14, 15, 16, 17, 18};
#else
  vec_usi_expected1 = (vector unsigned short){18, 17, 16, 15, 14, 13, 12, 11};
#endif

  disp = 2;
  vec_usi_result2 = vec_xl_be (disp, data_usi);

#ifdef __BIG_ENDIAN__
  vec_usi_expected2 = (vector unsigned short){12, 13, 14, 15, 16, 17, 18, 19};
#else
  vec_usi_expected2 = (vector unsigned short){19, 18, 17, 16, 15, 14, 13, 12};
#endif

  for (i = 0; i < 8; i++)
    {
      if (vec_ssi_result1[i] != vec_ssi_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_ssi_result1[%d] = %d; vec_ssi_expected1[%d] = %d\n",
	       i,  vec_ssi_result1[i], i, vec_ssi_expected1[i]);
#else
	abort ();
#endif

      if (vec_ssi_result2[i] != vec_ssi_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_ssi_result2[%d] = %d; vec_ssi_expected2[%d] = %d\n",
	       i,  vec_ssi_result2[i], i, vec_ssi_expected2[i]);
#else
	abort ();
#endif

      if (vec_usi_result1[i] != vec_usi_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_usi_result1[%d] = %d; vec_usi_expected1[%d] = %d\n",
	       i,  vec_usi_result1[i], i, vec_usi_expected1[i]);
#else
	abort ();
#endif

      if (vec_usi_result2[i] != vec_usi_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_usi_result2[%d] = %d; vec_usi_expected2[%d] = %d\n",
	       i,  vec_usi_result2[i], i, vec_usi_expected2[i]);
#else
	abort ();
#endif
    }

  vec_si_result1 = vec_xl_be (zero, data_si);

#ifdef __BIG_ENDIAN__
  vec_si_expected1 = (vector int){100, 101, 102, 103};
#else
  vec_si_expected1 = (vector int){103, 102, 101, 100};
#endif

  disp = 4;
  vec_si_result2 = vec_xl_be (disp, data_si);

#ifdef __BIG_ENDIAN__
  vec_si_expected2 = (vector int){101, 102, 103, 104};
#else
  vec_si_expected2 = (vector int){104, 103, 102, 101};
#endif

  vec_ui_result1 = vec_xl_be (zero, data_ui);

#ifdef __BIG_ENDIAN__
  vec_ui_expected1 = (vector unsigned int){101, 102, 103, 104};
#else
  vec_ui_expected1 = (vector unsigned int){104, 103, 102, 101};
#endif

  disp = 4;
  vec_ui_result2 = vec_xl_be (disp, data_ui);

#ifdef __BIG_ENDIAN__
  vec_ui_expected2 = (vector unsigned int){102, 103, 104, 105};
#else
  vec_ui_expected2 = (vector unsigned int){105, 104, 103, 102};
#endif


  for (i = 0; i < 4; i++)
    {
      if (vec_si_result1[i] != vec_si_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_si_result1[%d] = %d; vec_si_expected1[%d] = %d\n",
	       i,  vec_si_result1[i], i, vec_si_expected1[i]);
#else
	abort ();
#endif

      if (vec_si_result2[i] != vec_si_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_si_result2[%d] = %d; vec_si_expected2[%d] = %d\n",
	       i,  vec_si_result2[i], i, vec_si_expected2[i]);
#else
	abort ();
#endif

      if (vec_ui_result1[i] != vec_ui_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_ui_result1[%d] = %d; vec_ui_expected1[%d] = %d\n",
	       i,  vec_ui_result1[i], i, vec_ui_expected1[i]);
#else
	abort ();
#endif

      if (vec_ui_result2[i] != vec_ui_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_ui_result2[%d] = %d; vec_ui_expected2[%d] = %d\n",
	       i,  vec_ui_result2[i], i, vec_ui_expected2[i]);
#else
	abort ();
#endif
    }

  vec_sll_result1 = vec_xl_be (zero, data_sll);

#ifdef __BIG_ENDIAN__
  vec_sll_expected1 = (vector signed long long){1000, 1001};
#else
  vec_sll_expected1 = (vector signed long long){1001, 1000};
#endif

  disp = 8;
  vec_sll_result2 = vec_xl_be (disp, data_sll);

#ifdef __BIG_ENDIAN__
  vec_sll_expected2 = (vector signed long long){1001, 1002};
#else
  vec_sll_expected2 = (vector signed long long){1002, 1001};
#endif

  vec_ull_result1 = vec_xl_be (zero, data_ull);

#ifdef __BIG_ENDIAN__
  vec_ull_expected1 = (vector unsigned long long){1001, 1002};
#else
  vec_ull_expected1 = (vector unsigned long long){1002, 1001};
#endif

  disp = 8;
  vec_ull_result2 = vec_xl_be (disp, data_ull);

#ifdef __BIG_ENDIAN__
  vec_ull_expected2 = (vector unsigned long long){1002, 1003};
#else
  vec_ull_expected2 = (vector unsigned long long){1003, 1002};
#endif


  for (i = 0; i < 2; i++)
    {
      if (vec_sll_result1[i] != vec_sll_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_sll_result1[%d] = %lld; vec_sll_expected1[%d] = %d\n",
	       i,  vec_sll_result1[i], i, vec_sll_expected1[i]);
#else
	abort ();
#endif

      if (vec_sll_result2[i] != vec_sll_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_sll_result2[%d] = %lld; vec_sll_expected2[%d] = %d\n",
	       i,  vec_sll_result2[i], i, vec_sll_expected2[i]);
#else
	abort ();
#endif

      if (vec_ull_result1[i] != vec_ull_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_ull_result1[%d] = %lld; vec_ull_expected1[%d] = %d\n",
	       i,  vec_ull_result1[i], i, vec_ull_expected1[i]);
#else
	abort ();
#endif

      if (vec_ull_result2[i] != vec_ull_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_ull_result2[%d] = %lld; vec_ull_expected2[%d] = %d\n",
	       i,  vec_ull_result2[i], i, vec_sll_expected2[i]);
#else
	abort ();
#endif
    }

  vec_f_result1 = vec_xl_be (zero, data_f);

#ifdef __BIG_ENDIAN__
  vec_f_expected1 = (vector float){100000.0, 100001.0, 100002.0, 100003.0};
#else
  vec_f_expected1 = (vector float){100003.0, 100002.0, 100001.0, 100000.0};
#endif

  disp = 4;
  vec_f_result2 = vec_xl_be (disp, data_f);

#ifdef __BIG_ENDIAN__
  vec_f_expected2 = (vector float){100001.0, 100002.0, 100003.0, 100004.0};
#else
  vec_f_expected2 = (vector float){100004.0, 100003.0, 100002.0, 100001.0};
#endif

  for (i = 0; i < 4; i++)
    {
      if (vec_f_result1[i] != vec_f_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_f_result1[%d] = %f; vec_f_expected1[%d] = %f\n",
	       i,  vec_f_result1[i], i, vec_f_expected1[i]);
#else
	abort ();
#endif

      if (vec_f_result2[i] != vec_f_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_f_result2[%d] = %f; vec_f_expected2[%d] = %f\n",
	       i,  vec_f_result2[i], i, vec_f_expected2[i]);
#else
	abort ();
#endif
    }

  vec_d_result1 = vec_xl_be (zero, data_d);

#ifdef __BIG_ENDIAN__
  vec_d_expected1 = (vector double){1000000.0, 1000001.0};
#else
  vec_d_expected1 = (vector double){1000001.0, 1000000.0};
#endif

  disp = 8;
  vec_d_result2 = vec_xl_be (disp, data_d);

#ifdef __BIG_ENDIAN__
  vec_d_expected2 = (vector double){1000001.0, 1000002.0};
#else
  vec_d_expected2 = (vector double){1000002.0, 1000001.0};
#endif

  for (i = 0; i < 2; i++)
    {
      if (vec_d_result1[i] != vec_d_expected1[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_d_result2[%d] = %f; vec_d_expected2[%d] = %f\n",
	       i,  vec_d_result2[i], i, vec_d_expected2[i]);
#else
	abort ();
#endif

      if (vec_d_result2[i] != vec_d_expected2[i])
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_d_result2[%d] = %f; vec_d_expected2[%d] = %f\n",
	       i,  vec_d_result2[i], i, vec_d_expected2[i]);
#else
	abort ();
#endif
    }
}
