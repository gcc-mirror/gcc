long long unsigned
str2llu (str)
     char *str;
{
  long long unsigned acc;
  long long b = 10;
  char d;
  acc =  *str++ - '0';
  for (;;)
    {
      d = *str++;
      if (d == '\0')
	break;
      d -= '0';
      acc = acc * 10 + d;
    }

  return acc;
}
