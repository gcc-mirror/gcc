f()
{
  char  c1, c2;
  char *p1, *p2;

  do {
    c1 = c2 = *p1++;
    while (c1--)
      *p2++ = *p1++;
  } while (c2);
}
