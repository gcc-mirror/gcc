#include <stdio.h>
g ()
{
  return '\n';
}

f ()
{
  char s[] = "abcedfg01234";
  char *sp = s + 12;

  switch (g ())
    {
      case '\n':
        break;
    }

  while (*--sp == '0')
    ;
  sprintf (sp + 1, "X");

  if (s[12] != 'X')
    abort ();
}

main ()
{
  f ();
  exit (0);
}
