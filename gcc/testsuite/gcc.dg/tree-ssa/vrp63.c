/* PR tree-optimization/51721 */
/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error (void);

#define BITSM1 (sizeof (int) * __CHAR_BIT__ - 1)

void
f1 (unsigned int s)
{
  if (s >> 1 == 0)
    {
      if (s == 2 || s == -1U)
	link_error ();
    }
  else
    {
      if (s == 0 || s == 1)
	link_error ();
    }
}

void
f2 (unsigned int s)
{
  if (s >> 4 != 3)
    {
      if (s == 48 || s == 57 || s == 63)
	link_error ();
    }
  else
    {
      if (s == 47 || s == 64 || s == 0 || s == -1U)
	link_error ();
    }
}

void
f3 (int s)
{
  if (s >> 3 == -2)
    {
      if (s == -17 || s == -8 || s == 0
	  || s == -__INT_MAX__ - 1 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == -16 || s == -12 || s == -9)
	link_error ();
    }
}

void
f4 (unsigned int s)
{
  if (s >> 2 < 4)
    {
      if (s == 16 || s == 20 || s == -1U)
	link_error ();
    }
  else
    {
      if (s == 0 || s == 2 || s == 14 || s == 15)
	link_error ();
    }
}

void
f5 (unsigned int s)
{
  if (s >> 3 <= 7)
    {
      if (s == 64 || s == 68 || s == -1U)
	link_error ();
    }
  else
    {
      if (s == 0 || s == 1 || s == 62 || s == 63)
	link_error ();
    }
}

void
f6 (unsigned int s)
{
  if (s >> 1 > 2)
    {
      if (s == 0 || s == 3 || s == 5)
	link_error ();
    }
  else
    {
      if (s == 6 || s == 8 || s == -1U)
	link_error ();
    }
}

void
f7 (unsigned int s)
{
  if (s >> 5 >= 7)
    {
      if (s == 0 || s == 2 || s == 221 || s == 223)
	link_error ();
    }
  else
    {
      if (s == 224 || s == 256 || s == 258 || s == -1U)
	link_error ();
    }
}

void
f8 (int s)
{
  if (s >> 2 < -3)
    {
      if (s == -12 || s == -10 || s == 0 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == -13 || s == -16 || s == -__INT_MAX__ - 1)
	link_error ();
    }
}

void
f9 (int s)
{
  if (s >> 3 <= -2)
    {
      if (s == -8 || s == -6 || s == 0 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == -9 || s == -11 || s == -__INT_MAX__ - 1)
	link_error ();
    }
}

void
f10 (int s)
{
  if (s >> 1 > -4)
    {
      if (s == -7 || s == -9 || s == -__INT_MAX__ - 1)
	link_error ();
    }
  else
    {
      if (s == -6 || s == -4 || s == 0 || s == __INT_MAX__)
	link_error ();
    }
}

void
f11 (int s)
{
  if (s >> 3 >= -6)
    {
      if (s == -49 || s == -51 || s == -__INT_MAX__ - 1)
	link_error ();
    }
  else
    {
      if (s == -48 || s == -46 || s == 0 || s == __INT_MAX__)
	link_error ();
    }
}

void
f12 (int s)
{
  if (s >> 2 < 4)
    {
      if (s == 16 || s == 20 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == 0 || s == 2 || s == 14 || s == 15
	  || s == -2 || s == -__INT_MAX__ - 1)
	link_error ();
    }
}

void
f13 (int s)
{
  if (s >> 3 <= 7)
    {
      if (s == 64 || s == 68 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == 0 || s == 1 || s == 62 || s == 63
	  || s == -2 || s == -__INT_MAX__ - 1)
	link_error ();
    }
}

void
f14 (int s)
{
  if (s >> 1 > 2)
    {
      if (s == 0 || s == 3 || s == 5
	  || s == -2 || s == -__INT_MAX__ - 1)
	link_error ();
    }
  else
    {
      if (s == 6 || s == 8 || s == __INT_MAX__)
	link_error ();
    }
}

void
f15 (int s)
{
  if (s >> 5 >= 7)
    {
      if (s == 0 || s == 2 || s == 221 || s == 223
	  || s == -2 || s == -__INT_MAX__ - 1)
	link_error ();
    }
  else
    {
      if (s == 224 || s == 256 || s == 258 || s == __INT_MAX__)
	link_error ();
    }
}

unsigned int
f16 (unsigned int s)
{
  unsigned int t = s >> BITSM1;
  if (t != 0)
    {
      if (s == 0 || s == 5 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == 1U + __INT_MAX__ || s == 6U + __INT_MAX__ || s == -1U)
	link_error ();
    }
  return t;
}

int
f17 (int s)
{
  int t = s >> BITSM1;
  if (t == 0)
    {
      if (s == -1 || s == -5 || s == -__INT_MAX__ - 1)
	link_error ();
    }
  else
    {
      if (s == 0 || s == 5 || s == __INT_MAX__)
	link_error ();
    }
  return t;
}

unsigned int
f18 (unsigned int s)
{
  unsigned int t = s >> BITSM1;
  if (t >= 1)
    {
      if (s == 0 || s == 5 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == 1U + __INT_MAX__ || s == 6U + __INT_MAX__ || s == -1U)
	link_error ();
    }
  return t;
}

int
f19 (int s)
{
  int t = s >> BITSM1;
  if (t >= 0)
    {
      if (s == -1 || s == -5 || s == -__INT_MAX__ - 1)
	link_error ();
    }
  else
    {
      if (s == 0 || s == 5 || s == __INT_MAX__)
	link_error ();
    }
  return t;
}

unsigned int
f20 (unsigned int s)
{
  unsigned int t = s >> BITSM1;
  if (t < 1)
    {
      if (s == 1U + __INT_MAX__ || s == 6U + __INT_MAX__ || s == -1U)
	link_error ();
    }
  else
    {
      if (s == 0 || s == 5 || s == __INT_MAX__)
	link_error ();
    }
  return t;
}

int
f21 (int s)
{
  int t = s >> BITSM1;
  if (t < 0)
    {
      if (s == 0 || s == 5 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == -1 || s == -5 || s == -__INT_MAX__ - 1)
	link_error ();
    }
  return t;
}

int
main ()
{
  return 0;
}
