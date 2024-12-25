/* { dg-do run } */
/* { dg-options "-mcpu=21064" } */

extern void abort (void);

struct S2180
{
  char t;
  _Complex char u[4];
};

struct S2180 s2180;

int
main (void)
{
  volatile struct S2180 x;

  s2180.u[3] = 3 + 4i;

  x.u[3] = s2180.u[3];

  if (x.u[3] != s2180.u[3])
    abort ();

  return 0;
}
