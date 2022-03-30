/* { dg-do run } */
/* { dg-options "-O2 -fwhole-program" } */


float val2 = 1.710780f;
float val3;
volatile float vf;

int __attribute__((noipa))
get_bool (void)
{
  return 1;
}

int __attribute__((noinline))
wrong (float *pos)
{
  _Complex float a;

  __real__ a = *pos;
  __imag__ a = *pos;

  _Complex float b = 0 + 0i;

  b = b + a;

  if (b == 0.0f)
    return 1;

  vf = __imag__ b;
  return 0;
}

int main(int argc, char **argv) {
  float val = get_bool () == 1 ? val2 : val3;

  if ((wrong(&val), wrong(&val)))
    __builtin_abort ();
  return 0;
}
