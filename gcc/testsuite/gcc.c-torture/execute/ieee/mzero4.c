/* Copyright (C) 2003  Free Software Foundation.
   by Roger Sayle <roger@eyesopen.com>, derived from mzero3.c

   Constant folding of sin(-0.0), tan(-0.0) and atan(-0.0) should
   all return -0.0, for both double and float forms.  */

void abort (void);
typedef __SIZE_TYPE__ size_t;
extern int memcmp (const void *, const void *, size_t);

double sin (double);
double tan (double);
double atan (double);

float sinf (float);
float tanf (float);
float atanf (float);

void expectd (double, double);
void expectf (float, float);

void
expectd (double value, double expected)
{
  if (value != expected
      || memcmp ((void *)&value, (void *) &expected, sizeof (double)) != 0)
    abort ();
}

void
expectf (float value, float expected)
{
  if (value != expected
      || memcmp ((void *)&value, (void *) &expected, sizeof (float)) != 0)
    abort ();
}

int main ()
{
  expectd (sin (0.0), 0.0);
  expectd (tan (0.0), 0.0);
  expectd (atan (0.0), 0.0);

  expectd (sin (-0.0), -0.0);
  expectd (tan (-0.0), -0.0);
  expectd (atan (-0.0), -0.0);

  expectf (sinf (0.0f), 0.0f);
  expectf (tanf (0.0f), 0.0f);
  expectf (atanf (0.0f), 0.0f);

  expectf (sinf (-0.0f), -0.0f);
  expectf (tanf (-0.0f), -0.0f);
  expectf (atanf (-0.0f), -0.0f);

  return 0;
}

