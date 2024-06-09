void
f (const char *s, char *d, unsigned l)
{
  if (0)
    while (1);
  else
    while (--l >= 0)
      *d++ = *s++;
}
