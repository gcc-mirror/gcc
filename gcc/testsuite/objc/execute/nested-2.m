/* Contributed by Nicola Pero Mon Mar  5 19:57:11 CET 2001 */

int main (void)
{
  inline int nested (void)
    {
      return 1;
    }

  if (nested () != 1)
    {
      exit (1);
    }

  return 0;
}

