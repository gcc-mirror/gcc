/* PR tree-optimization/51721 */
/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error (void);

#define BITSM1 (sizeof (int) * __CHAR_BIT__ - 1)

void
f1 (unsigned int s)
{
  if (s >> BITSM1 != 0)
    {
      if (s == 0 || s == 5 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == 1U + __INT_MAX__ || s == 6U + __INT_MAX__ || s == -1U)
	link_error ();
    }
}

void
f2 (int s)
{
  if (s >> BITSM1 == 0)
    {
      if (s == -1 || s == -5 || s == -__INT_MAX__ - 1)
	link_error ();
    }
  else
    {
      if (s == 0 || s == 5 || s == __INT_MAX__)
	link_error ();
    }
}

void
f3 (unsigned int s)
{
  if ((s & (1U << BITSM1)) != 0)
    {
      if (s == 0 || s == 5 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == 1U + __INT_MAX__ || s == 6U + __INT_MAX__ || s == -1U)
	link_error ();
    }
}

void
f4 (int s)
{
  if ((s & (1U << BITSM1)) == 0)
    {
      if (s == -1 || s == -5 || s == -__INT_MAX__ - 1)
	link_error ();
    }
  else
    {
      if (s == 0 || s == 5 || s == __INT_MAX__)
	link_error ();
    }
}

void
f5 (unsigned int s)
{
  if ((int) s < 0)
    {
      if (s == 0 || s == 5 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == 1U + __INT_MAX__ || s == 6U + __INT_MAX__ || s == -1U)
	link_error ();
    }
}

void
f6 (unsigned int s)
{
  if ((int) s < 4)
    {
      if (s == 4 || s == 6 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == 1U + __INT_MAX__ || s == 6U + __INT_MAX__ || s == -1U
	  || s == 3 || s == 0)
	link_error ();
    }
}

void
f7 (unsigned int s)
{
  if ((int) s <= -7)
    {
      if (s == -6U || s == -1U || s == 0 || s == 4 || s == 6 || s == __INT_MAX__)
	link_error ();
    }
  else
    {
      if (s == 1U + __INT_MAX__ || s == 6U + __INT_MAX__ || s == -9U
	  || s == -7U)
	link_error ();
    }
}

void
f8 (unsigned int s)
{
  if ((int) s >= 4)
    {
      if (s == 1U + __INT_MAX__ || s == 6U + __INT_MAX__ || s == -1U
	  || s == 3 || s == 0)
	link_error ();
    }
  else
    {
      if (s == 4 || s == 6 || s == __INT_MAX__)
	link_error ();
    }
}

void
f9 (unsigned int s)
{
  if ((int) s > -7)
    {
      if (s == 1U + __INT_MAX__ || s == 6U + __INT_MAX__ || s == -9U
	  || s == -7U)
	link_error ();
    }
  else
    {
      if (s == -6U || s == -1U || s == 0 || s == 4 || s == 6 || s == __INT_MAX__)
	link_error ();
    }
}

int
main ()
{
  return 0;
}
