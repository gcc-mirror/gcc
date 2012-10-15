/* PR target/54925  */
extern int bar;
extern void foo (int *);
static unsigned char *
nr_memcpy (unsigned char *, unsigned char *, unsigned short);

void 
baz (char *buf, unsigned short len)
{
  unsigned char data[10];
  if (len == 0)
    return;
  nr_memcpy (data, (unsigned char *) buf, len);
  foo (&bar);
}

static unsigned char *
nr_memcpy (unsigned char * to, unsigned char * from, unsigned short len)
{
  unsigned char *p = to;
  while (len > 0)
    {
      len--;
      *to++ = *from++;
    }
  return p;
}
