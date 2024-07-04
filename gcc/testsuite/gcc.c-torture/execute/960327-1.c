#include <stdio.h>

void abort (void);
void exit (int);

int
g (void)
{
  return '\n';
}

void
f (void)
{
  char s[] = "abcedfg012345";
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

int
main (void)
{
  f ();
  exit (0);
}
