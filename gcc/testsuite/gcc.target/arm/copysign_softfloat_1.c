/* { dg-do run } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-additional-options "-mthumb -O2 --save-temps" } */

extern void abort (void);

#define N 16

float a_f[N] = {-0.1f, -3.2f, -6.3f, -9.4f,
		-12.5f, -15.6f, -18.7f, -21.8f,
		24.9f, 27.1f, 30.2f, 33.3f,
		36.4f, 39.5f, 42.6f, 45.7f};

float b_f[N] = {-1.2f, 3.4f, -5.6f, 7.8f,
		-9.0f, 1.0f, -2.0f, 3.0f,
		-4.0f, -5.0f, 6.0f, 7.0f,
		-8.0f, -9.0f, 10.0f, 11.0f};

float c_f[N] = {-0.1f, 3.2f, -6.3f, 9.4f,
		-12.5f, 15.6f, -18.7f, 21.8f,
		-24.9f, -27.1f, 30.2f, 33.3f,
		-36.4f, -39.5f, 42.6f, 45.7f};

double a_d[N] = {-0.1, -3.2, -6.3, -9.4,
		 -12.5, -15.6, -18.7, -21.8,
		 24.9, 27.1, 30.2, 33.3,
		 36.4, 39.5, 42.6, 45.7};

double b_d[N] = {-1.2, 3.4, -5.6, 7.8,
		 -9.0, 1.0, -2.0, 3.0,
		 -4.0, -5.0, 6.0, 7.0,
		 -8.0, -9.0, 10.0, 11.0};

double c_d[N] = {-0.1, 3.2, -6.3, 9.4,
		 -12.5, 15.6, -18.7, 21.8,
		 -24.9, -27.1, 30.2, 33.3,
		 -36.4, -39.5, 42.6, 45.7};

int
main (int argc, char **argv)
{
  int index = 0;

/* { dg-final { scan-assembler-times "bfi" 2 { target arm_softfloat } } } */
  for (index; index < N; index++)
    {
      if (__builtin_copysignf (a_f[index], b_f[index]) != c_f[index])
	abort();
    }

  for (index = 0; index < N; index++)
    {
      if (__builtin_copysign (a_d[index], b_d[index]) != c_d[index])
	abort();
    }

  return 0;
}

