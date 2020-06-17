/* PR middle-end/82612 - missing -Warray-bounds on a non-zero offset
   from the address of a non-array object
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

int i;
int f0 (void)
{
  int *p = &i;
  return p[2];      // { dg-warning "-Warray-bounds" }
}

int f1 (int j)
{
  int i = j;
  int *p = &i;
  return p[2];      // { dg-warning "-Warray-bounds" }
}

int f2 (int i)
{
  int *p = &i;
  return p[2];      // { dg-warning "-Warray-bounds" }
}

/* { dg-prune-output "-Wuninitialized" } */
