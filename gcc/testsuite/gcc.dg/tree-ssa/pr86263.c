/* { dg-do compile } */

void f1 (void *);
void f2 (void *);
void f3 (void *);
void f4 (void *);

char
_dcvt (void *ptr, char type, int opt, int val)
{
  switch (type)
    {
    case 'f':
      f4 (ptr);
    case 'F':
      f1 (ptr);
      break;
    case 'g':
    case 'G':
      if (opt == 0)
	opt = 1;
      f2 (ptr);
      break;
    case 'e':
    case 'E':
      f3 (ptr);
    }
  return val;
}
