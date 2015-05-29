/* PR rtl-optimization/38245 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern void link_error (void);

void
f1 (unsigned int a)
{
  if (a != 28)
    {
      if (4 / a == 5)
	link_error ();
    }
}

void
f2 (unsigned int a)
{
  if (4 / a == 5)
    link_error ();
}

void
f3 (unsigned int a)
{
  if (4 / (a & 0xff) == 5)
    link_error ();
}

void
f4 (unsigned int a, unsigned int b)
{
  if ((b & 3) / ((a & 0xff) + 1) == 5)
    link_error ();
}

void
f5 (int a)
{
  if (a != 28)
    {
      if (4 / a == 5)
	link_error ();
    }
}

void
f6 (int a)
{
  if (4 / a == 5)
    link_error ();
}

void
f7 (int a)
{
  if (4 / (a & 0xff) == 5)
    link_error ();
}

void
f8 (int a, int b)
{
  if ((b & 3) / ((a & 0xff) + 1) == 5)
    link_error ();
}

void
f9 (int a, int b)
{
  if (b >= 4)
    if ((a / b) == __INT_MAX__ / 2)
      link_error ();
}

void
f10 (unsigned int a, unsigned int b)
{
  if (b >= 16)
    if ((a / b) == __INT_MAX__ / 4)
      link_error ();
}

void
f11 (int a, int b)
{
  if (b <= -32)
    if ((a / b) == -__INT_MAX__ / 16)
      link_error ();
}

void
f12 (int a, int b)
{
  if (a >= -6 && a <= 4)
    if ((a / b) == -7 || (a / b) == 7)
      link_error ();
}

void
f13 (unsigned int a, unsigned int b)
{
  if (a <= 4)
    if ((a / b) == 5)
      link_error ();
}

/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */
