/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches" } */

int
f (int p)
{
  if (p == 0)
    {
      p += 1, ++p;
    }
  else
    {
      p -= 1, ++p;
    }

  if (p == 1)
    {
    }
  else
    p++;

  if (p == 2)
    p++;
  else
    {
    }

  if (p == 3)
    {
    }
  else
    {
    }

  if (p == 4)
    {
      ++p;
      return p;
    }
  else
    {
      p++;
      return p;
    }

  if (p == 5)
    ++p;
  else
    p++;

  if (p == 6)
    {
      ++p;
      ++p;
      return p;
    }
  else
    {
      ++p;
      return p;
    }

  if (p == 7)
    {
      ++p;
      return p;
    }
  else
    {
      ++p;
      ++p;
      return p;
    }
}
