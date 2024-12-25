/* { dg-do run } */
/* { dg-options "-mcpu=21064" } */

extern void abort (void);

struct S2180
{
  char t;
  _Complex char u[2];
};

struct S2180 s2180;

int
main (void)
{
  volatile struct S2180 x;

  s2180.u[1] = 3 + 4i;

  x.u[1] = s2180.u[1];

  if (x.u[1] != s2180.u[1])
    abort ();

  return 0;
}
