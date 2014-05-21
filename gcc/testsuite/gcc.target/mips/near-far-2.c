/* { dg-do compile } */
/* { dg-options "-mno-long-calls addressing=absolute" } */

extern int long_call_func () __attribute__((long_call));
extern int far_func () __attribute__((far));
extern int near_func () __attribute__((near));
extern int normal_func ();

int test ()
{
  return (long_call_func ()
          + far_func ()
          + near_func ()
          + normal_func ());
}

/* { dg-final { scan-assembler-not "\tjal(|s)\tlong_call_func\n" } } */
/* { dg-final { scan-assembler-not "\tjal(|s)\tfar_func\n" } } */
/* { dg-final { scan-assembler     "\tjal(|s)\tnear_func\n" } } */
/* { dg-final { scan-assembler     "\tjal(|s)\tnormal_func\n" } } */
