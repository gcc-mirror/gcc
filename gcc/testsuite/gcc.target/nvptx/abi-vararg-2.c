/* { dg-do run } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long -fshort-enums" } */

/* va args, promoted as regular knr args.  */

void __attribute__ ((noinline)) chars (int i, ...)
{
  __builtin_va_list args;
  __builtin_va_start (args, i);

  if (__builtin_va_arg (args, int) != 1) __builtin_abort ();
  if (__builtin_va_arg (args, int) != 2) __builtin_abort ();
  __builtin_va_end (args);
}

void __attribute__ ((noinline)) shorts (int i, ...)
{
  __builtin_va_list args;
  __builtin_va_start (args, i);

  if (__builtin_va_arg (args, int) != 1) __builtin_abort ();
  if (__builtin_va_arg (args, int) != 2) __builtin_abort ();
  if (__builtin_va_arg (args, int) != 3) __builtin_abort ();
  __builtin_va_end (args);
}

void __attribute__ ((noinline)) ints (int i, ...)
{
  __builtin_va_list args;
  __builtin_va_start (args, i);

  if (__builtin_va_arg (args, int) != 1) __builtin_abort ();
  if (__builtin_va_arg (args, int) != 2) __builtin_abort ();
  if (__builtin_va_arg (args, int) != 3) __builtin_abort ();
  __builtin_va_end (args);
}

void __attribute__ ((noinline)) longlongs (int i, ...)
{
  __builtin_va_list args;
  __builtin_va_start (args, i);

  if (__builtin_va_arg (args, int) != 1) __builtin_abort ();
  if (__builtin_va_arg (args, long long) != 2) __builtin_abort ();
  if (__builtin_va_arg (args, long long) != 3) __builtin_abort ();
  __builtin_va_end (args);
}

typedef int __attribute__ ((mode(TI))) ti;

void __attribute__ ((noinline)) tis (int i, ...)
{
  __builtin_va_list args;
  __builtin_va_start (args, i);

  if (__builtin_va_arg (args, int) != 1) __builtin_abort ();
  if (__builtin_va_arg (args, ti) != 2) __builtin_abort ();
  if (__builtin_va_arg (args, ti) != 3) __builtin_abort ();
  __builtin_va_end (args);
}

void __attribute__ ((noinline)) floats (int i, ...)
{
  __builtin_va_list args;
  __builtin_va_start (args, i);

  if (__builtin_va_arg (args, int) != 1) __builtin_abort ();
  if (__builtin_va_arg (args, double) != 2) __builtin_abort ();
  if (__builtin_va_arg (args, double) != 3) __builtin_abort ();
  __builtin_va_end (args);
}

void __attribute__ ((noinline)) doubles (int i, ...)
{
  __builtin_va_list args;
  __builtin_va_start (args, i);

  if (__builtin_va_arg (args, int) != 1) __builtin_abort ();
  if (__builtin_va_arg (args, double) != 2) __builtin_abort ();
  if (__builtin_va_arg (args, double) != 3) __builtin_abort ();
  __builtin_va_end (args);
}

typedef enum {ec1, ec2, ecmax = 0xf} echar;
typedef enum {es1, es2, esmax = 0xfff} eshort;
typedef enum {ei1, ei2, eimax = 0xfffff} eint;

void __attribute__ ((noinline)) echars (int i, ...)
{
  __builtin_va_list args;
  __builtin_va_start (args, i);

  if (__builtin_va_arg (args, int) != ec1) __builtin_abort ();
  if (__builtin_va_arg (args, int) != ec2) __builtin_abort ();
  if (__builtin_va_arg (args, int) != ecmax) __builtin_abort ();
  __builtin_va_end (args);
}

void __attribute__ ((noinline)) eshorts (int i, ...)
{
  __builtin_va_list args;
  __builtin_va_start (args, i);

  if (__builtin_va_arg (args, int) != es1) __builtin_abort ();
  if (__builtin_va_arg (args, int) != es2) __builtin_abort ();
  if (__builtin_va_arg (args, int) != esmax) __builtin_abort ();
  __builtin_va_end (args);
}

void __attribute__ ((noinline)) eints (int i, ...)
{
  __builtin_va_list args;
  __builtin_va_start (args, i);

  if (__builtin_va_arg (args, int) != ei1) __builtin_abort ();
  if (__builtin_va_arg (args, int) != ei2) __builtin_abort ();
  if (__builtin_va_arg (args, int) != eimax) __builtin_abort ();
  __builtin_va_end (args);
}

typedef struct {char a;} one;
typedef struct {short a;} two;
typedef struct {int a;} four;
typedef struct {long long a;} eight;
typedef struct {int a, b[12];} big;

void __attribute__ ((noinline)) structs (int i, ...)
{
  __builtin_va_list args;
  __builtin_va_start (args, i);

  if (__builtin_va_arg (args, int) != 1) __builtin_abort ();
  if (__builtin_va_arg (args, one).a != 2) __builtin_abort ();
  if (__builtin_va_arg (args, int) != 3) __builtin_abort ();
  if (__builtin_va_arg (args, two).a != 4) __builtin_abort ();
  if (__builtin_va_arg (args, int) != 5) __builtin_abort ();
  if (__builtin_va_arg (args, four).a != 6) __builtin_abort ();
  if (__builtin_va_arg (args, int) != 7) __builtin_abort ();
  if (__builtin_va_arg (args, eight).a != 8) __builtin_abort ();
  if (__builtin_va_arg (args, int) != 9) __builtin_abort ();
  if (__builtin_va_arg (args, big).a != 10) __builtin_abort ();
  __builtin_va_end (args);
}

int main ()
{
  char vc1, vc2, vc3, vc4, vc5;
  short vs1, vs2, vs3;
  int vi1, vi2, vi3;
  long long vll1, vll2;
  ti vti1, vti2;
  float vf1, vf2;
  double vd1, vd2;

  one vone;
  two vtwo;
  four vfour;
  eight veight;
  big vbig;
  echar vec1, vec2,  vec3;
  eshort ves1, ves2,  ves3;
  eint vei1, vei2,  vei3;

  vc1 = 1, vc2 = 2;
  chars (1, vc1, vc2);

  vc1 = 1, vs1 = 2, vs2 = 3;
  shorts (1, vc1, vs1, vs2);

  vc1 = 1, vi1 = 2, vi2 = 3;
  ints (1, vc1, vi1, vi2);

  vc1 = 1,  vll1 = 2, vll2 = 3;
  longlongs (1, vc1, vll1, vll2);

  vc1 = 1, vti1 = 2, vti2 = 3;
  tis (1, vc1,  vti1, vti2);

  vc1 = 1,  vf1 = 2, vf2 = 3;
  floats (1, vc1, vf1, vf2);
  doubles (1, vc1, vf1, vf2); /* Floats are promoted, so this should work. */

  vc1 = 1, vd1 = 2, vd2 = 3;
  floats (1, vc1, vf1, vf2); /* Floats are promoted, so this should work. */
  doubles (1, vc1, vf1, vf2);

  vec1 = ec1, vec2 = ec2, vec3 = ecmax;
  echars (1, vec1, vec2, vec3);
  vc1 = ec1, vc2 = ec2, vc3 = ecmax;
  echars (1, vc1, vc2, vc3);

  ves1 = ec1, ves2 = ec2, ves3 = esmax;
  eshorts (1, ves1, ves2, ves3);
  vs1 = ec1, vs2 = ec2, vs3 = esmax;
  eshorts (1, vs1, vs2, vs3);

  vei1 = ec1, vei2 = ec2, vei3 = eimax;
  eints (1, vei1, vei2, vei3);
  vi1 = ec1, vi2 = ec2, vi3 = eimax;
  eints (1, vi1, vi2, vi3);

  vc1 = 1, vone.a = 2, vc2 = 3, vtwo.a = 4,
    vc3 = 5, vfour.a = 6, vc4 = 7, veight.a = 8,
    vc5 = 9, vbig.a = 10;
  structs (1, vc1,  vone, vc2, vtwo, vc3, vfour, vc4, veight, vc5, vbig);
  
  return 0;
}
