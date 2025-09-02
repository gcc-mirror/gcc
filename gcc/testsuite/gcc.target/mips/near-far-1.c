/* { dg-do compile } */
/* { dg-options "-mlong-calls addressing=absolute" } */

extern int long_call_func () __attribute__((long_call));
extern int far_func () __attribute__((far));
extern int short_call_func () __attribute__((short_call));
extern int near_func () __attribute__((near));
extern int normal_func ();

int test ()
{
  return (long_call_func ()
          + far_func ()
          + short_call_func ()
          + near_func ()
          + normal_func ());
}

/* { dg-final { scan-assembler-not "\t(jals?|balc)\tlong_call_func\n" } } */
/* { dg-final { scan-assembler-not "\t(jals?|balc)\tfar_func\n" } } */
/* { dg-final { scan-assembler     "\t(jals?|balc)\tshort_call_func\n" } } */
/* { dg-final { scan-assembler     "\t(jals?|balc)\tnear_func\n" } } */
/* { dg-final { scan-assembler-not "\t(jals?|balc)\tnormal_func\n" } } */
