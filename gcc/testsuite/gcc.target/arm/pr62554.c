/* Check that pre ARMv4 compilation still works.  */
/* { dg-do compile } */
/* { dg-options "-marm -march=armv3 -O" } */
/* { dg-require-effective-target arm_arm_ok } */

typedef struct
{
  char bits;
  short val;
} code;

union uu
{
  short us;
  char b[2];
};

int a, b, c, f, g, h;
code *d;

code e;

int
fn1 (void)
{
  char i;
  do
    if (e.bits)
      {
      dodist:
        f = c;
        if (e.bits & 6)
          {
            ++i;
            if (g)
              do
                {
                  union uu j;
                  j.b[1] = a;
                  h = j.us;
                }
              while (fn1);
          }
        else
          {
            e = d[b];
            goto dodist;
          }
      }
  while (i);
}
