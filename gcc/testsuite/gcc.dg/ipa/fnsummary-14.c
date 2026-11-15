/* { dg-do compile } */
/* { dg-require-effective-target label_values } */
/* { dg-options "-O2 -fno-early-inlining" } */

void cold_work (void);

static int
check (const char *p)
{
  if (p[0] == 0)
    cold_work ();
  return 0;
}

int
caller (void)
{
lbl:
  return check ((const char *) &&lbl);
}
