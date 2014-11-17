/* Test Thumb1 insn pattern addsi3_cbranch_scratch.  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { ! { arm_thumb1 } } } */

struct real_value {

  unsigned int cl : 2;
  unsigned int decimal : 1;
  unsigned int sign : 1;
  unsigned int signalling : 1;
  unsigned int canonical : 1;
  unsigned int uexp : (32 - 6);
  unsigned long sig[((128 + (8 * 4)) / (8 * 4))];
};

enum real_value_class {
      rvc_zero,
      rvc_normal,
      rvc_inf,
      rvc_nan
};

extern void exit(int);
extern int foo(long long *, int, int);

int
real_to_integer (const struct real_value *r, int *fail, int precision)
{
  long long val[2 * (((64*(8)) + 64) / 64)];
  int exp;
  int words, w;
  int result;

  switch (r->cl)
    {
    case rvc_zero:
    underflow:
      return 100;

    case rvc_inf:
    case rvc_nan:
    overflow:
      *fail = 1;

      if (r->sign)
 return 200;
      else
 return 300;

    case rvc_normal:
      if (r->decimal)
 return 400;

      exp = ((int)((r)->uexp ^ (unsigned int)(1 << ((32 - 6) - 1))) - (1 << ((32 - 6) - 1)));
      if (exp <= 0)
 goto underflow;


      if (exp > precision)
 goto overflow;
      words = (precision + 64 - 1) / 64;
      w = words * 64;
      for (int i = 0; i < words; i++)
 {
   int j = ((128 + (8 * 4)) / (8 * 4)) - (words * 2) + (i * 2);
   if (j < 0)
     val[i] = 0;
   else
     val[i] = r->sig[j];
   j += 1;
   if (j >= 0)
     val[i] |= (unsigned long long) r->sig[j] << (8 * 4);
 }


      result = foo(val, words, w);

      if (r->sign)
 return -result;
      else
 return result;

    default:
      exit(2);
    }
}

