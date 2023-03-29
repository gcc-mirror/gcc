/* { dg-do compile } */
/* { dg-options "-O2 -Wuse-after-free=2" } */

unsigned long bufmax = 0;
unsigned long __open_catalog_bufmax;
void *realloc(void *, __SIZE_TYPE__);
void free(void *);

void __open_catalog(char *buf)
{
  char *old_buf = buf;
  buf = realloc (buf, bufmax);
  if (__builtin_expect ((buf == ((void *)0)), 0))
    free (old_buf); /* { dg-bogus "used after" } */
}
