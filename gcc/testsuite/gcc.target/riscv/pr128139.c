/* { dg-do compile } */

void b(char *);

void a(long off, void *h)
{
  char buf[8192];
  b(buf);
  __builtin_eh_return (off, h);
}


