/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca-larger-than=5000 -O2" } */

extern void useit(char *);

int
foobar (unsigned short length)
{
  char *pbuf;
  __SIZE_TYPE__ size = (__SIZE_TYPE__) length;

  if (size < 4032)
    pbuf = (char *) __builtin_alloca(size);
  else
    pbuf = (char *) __builtin_malloc (size);

  useit(pbuf);
  return 0;
}
