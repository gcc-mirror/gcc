static const char * const lcset = "0123456789abcdef";
static const char * const ucset = "0123456789ABCDEF";

char *
f (char *buffer, long long value, char type)
{
  int base, i;

  i = 128  - 1;
  buffer[i--] = '\0';

  switch (type)
    {
    case 'u':
    case 'o':
    case 'x':
    case 'X':
      if (type == 'u')
	base = 10;
      else if (type == 'o')
	base = 8;
      else
	base = 16;

      while (i >= 0)
	{
	  if (type == 'X')
	    buffer[i--] = ucset[((unsigned long long) value) % base];
	  else
	    buffer[i--] = lcset[((unsigned long long) value) % base];

	  if ((value = ((unsigned long long) value) / base) == 0)
	    break;
	}
      break;
    }

  return &buffer[++i];
}
