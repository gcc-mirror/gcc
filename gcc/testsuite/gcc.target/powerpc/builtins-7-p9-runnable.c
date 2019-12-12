/* { dg-do run { target { powerpc*-*-* && p9vector_hw } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#include <altivec.h>
#define TRUE 1
#define FALSE 0

#ifdef DEBUG
#include <stdio.h>
#endif

#define EXTRACT 0

void abort (void);

int result_wrong_ull (vector unsigned long long vec_expected,
		      vector unsigned long long vec_actual)
{
  int i;

  for (i = 0; i < 2; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

int result_wrong_uc (vector unsigned char vec_expected,
		     vector unsigned char vec_actual)
{
  int i;

  for (i = 0; i < 16; i++)
    if (vec_expected[i] != vec_actual[i])
      return TRUE;

  return FALSE;
}

#ifdef DEBUG
void print_ull (vector unsigned long long vec_expected,
		vector unsigned long long vec_actual)
{
  int i;

  printf("expected unsigned long long data\n");
  for (i = 0; i < 2; i++)
    printf(" %lld,", vec_expected[i]);

  printf("\nactual signed char data\n");
  for (i = 0; i < 2; i++)
    printf(" %lld,", vec_actual[i]);
  printf("\n");
}

void print_uc (vector unsigned char vec_expected,
	       vector unsigned char vec_actual)
{
  int i;

  printf("expected unsigned char data\n");
  for (i = 0; i < 16; i++)
    printf(" %d,", vec_expected[i]);

  printf("\nactual unsigned char data\n");
  for (i = 0; i < 16; i++)
    printf(" %d,", vec_actual[i]);
  printf("\n");
}
#endif

#if EXTRACT
vector unsigned long long
vext (vector unsigned char *vc)
{
  return vextract_si_vchar (*vc, 5);
}
#endif

int main()
{
   vector signed int vsi_arg;
   vector unsigned int vui_arg;
   vector unsigned char vec_uc_arg, vec_uc_result, vec_uc_expected;
   vector unsigned long long vec_ull_result, vec_ull_expected;
   unsigned long long ull_result, ull_expected;

   vec_uc_arg = (vector unsigned char){1, 2, 3, 4,
				       5, 6, 7, 8,
				       9, 10, 11, 12,
				       13, 14, 15, 16};

   vsi_arg = (vector signed int){0xA, 0xB, 0xC, 0xD};

   vec_uc_expected = (vector unsigned char){0xC, 0, 0, 0,
					    5, 6, 7, 8,
					    9, 10, 11, 12,
					    13, 14, 15, 16};
   /* Test vec_insert4b() */
   /* Insert into char 0 location */
   vec_uc_result = vec_insert4b (vsi_arg, vec_uc_arg, 0);

   if (result_wrong_uc(vec_uc_expected, vec_uc_result))
     {
#ifdef DEBUG
        printf("Error: vec_insert4b pos 0, result does not match expected result\n");
	print_uc (vec_uc_expected, vec_uc_result);
#else
        abort();
#endif
      }

   /* insert into char 4 location */
   vec_uc_expected = (vector unsigned char){1, 2, 3, 4,
					    2, 0, 0, 0,
					    9, 10, 11, 12,
					    13, 14, 15, 16};
   vui_arg = (vector unsigned int){0x4, 0x3, 0x2, 0x1};

   vec_uc_result = vec_insert4b (vui_arg, vec_uc_arg, 4);

   if (result_wrong_uc(vec_uc_expected, vec_uc_result))
     {
#ifdef DEBUG
        printf("Error: vec_insert4b pos 4, result does not match expected result\n");
	print_uc (vec_uc_expected, vec_uc_result);
#else
        abort();
#endif
      }

   /* Test vec_extract4b() */
   /* Extract 4b, from char 0 location */
   vec_uc_arg = (vector unsigned char){10, 0, 0, 0,
				       20, 0, 0, 0,
				       30, 0, 0, 0,
				       40, 0, 0, 0};

   vec_ull_expected = (vector unsigned long long){0, 10};
   vec_ull_result = vec_extract4b(vec_uc_arg, 0);

   if (result_wrong_ull(vec_ull_expected, vec_ull_result))
     {
#ifdef DEBUG
        printf("Error: vec_extract4b pos 0, result does not match expected result\n");
	print_ull (vec_ull_expected, vec_ull_result);
#else
        abort();
#endif
      }

   /* Extract 4b, from char 12 location */
   vec_uc_arg = (vector unsigned char){10, 0, 0, 0,
				       20, 0, 0, 0,
				       30, 0, 0, 0,
				       40, 0, 0, 0};

   vec_ull_expected = (vector unsigned long long){0, 40};
   vec_ull_result = vec_extract4b(vec_uc_arg, 12);

   if (result_wrong_ull(vec_ull_expected, vec_ull_result))
     {
#ifdef DEBUG
        printf("Error: vec_extract4b pos 12, result does not match expected result\n");
	print_ull (vec_ull_expected, vec_ull_result);
#else
        abort();
#endif
      }
}
