int *
main (s1, s2)
     int *s1; int *s2;
{
  while ((*s1++ = *s2++) != '\0')
    ;
  return s1 - 1;
}
