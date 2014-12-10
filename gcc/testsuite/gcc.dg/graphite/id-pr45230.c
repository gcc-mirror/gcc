unsigned char buf[10];
int
main ()
{
  unsigned off1, len, i;
  unsigned char *p1;
  for (len = 0; len < 8; len++)
    {
      p1 = buf;
      for (i = 0; i < off1; i++)
	*p1++ = '\0';
      for (i = 0; i < len; i++)
	*p1++ = 'a';
    }
}
